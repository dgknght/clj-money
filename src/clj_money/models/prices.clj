(ns clj-money.models.prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.settings :as settings]))

(def ^:dynamic *skip-account-updates* false)

(defn- trade-date-unique?
  [{:keys [id] :as price}]
  (zero?
    (-> price
        (select-keys [:price/commodity
                      :price/trade-date])
        (assoc-if :id (when id [:!= id]))
        (db/model-type :price)
        models/count)))
(v/reg-spec trade-date-unique? {:message "%s already exists"
                                :path [:price/trade-date]})

(s/def :price/commodity ::models/model-ref)
(s/def :price/trade-date t/local-date?)
(s/def :price/price decimal?)
(s/def ::id uuid?)
(s/def ::models/price (s/and (s/keys :req [:price/commodity
                                           :price/trade-date
                                           :price/price]
                                     :opt [::id])
                             trade-date-unique?))

(defn- update-commodity
  [{:price/keys [commodity trade-date] :as price}]
  (-> commodity
      (update-in [:commodity/earliest-price] dates/earliest trade-date)
      (update-in [:commodity/latest-price] dates/latest trade-date)
      (models/put))
  price)

(defn- update-entity
  [{:price/keys [trade-date commodity] :as price}]
  (-> (models/find (:id (:commodity/entity commodity)) :entity)
      (update-in [:entity/settings :settings/earliest-price-date] dates/earliest trade-date)
      (update-in [:entity/settings :settings/latest-price-date] dates/latest trade-date)
      (models/put))
  price)

(defn- update-account
  [account {:price/keys [trade-date price]}]
  (-> account
      (assoc :account/price-as-of trade-date
             :account/value (* (get-in account [:account/quantity] 0M)
                               price))
      (models/put)))

(defn- accounts-to-update
  [{:price/keys [commodity trade-date]} {:keys [price-as-of]}]
  (let [criteria (if price-as-of
                   [:and {:account/commodity commodity
                          :account/quantity [:> 0M]}
                    [:or
                     {:account/price-as-of [:< trade-date]}
                     {:account/price-as-of nil}]]
                   {:account/commodity commodity
                    :account/price-as-of price-as-of})]
    (models/select criteria)))

(defn update-accounts
  [price & opts]
  (when-not *skip-account-updates*
    (doseq [a (accounts-to-update price opts)]
      (update-account a price)))
  price)

(defmethod models/after-save :price
  [price]
  (-> price
      (update-in [:price/commodity] (models/find :commodity))
      update-entity
      update-commodity
      update-accounts))

(defn most-recent
  ([commodity]
   (most-recent commodity nil))
  ([commodity as-of]
   {:pre [(map? commodity)]}

   (let [{:commodity/keys [earliest-price
                           latest-price]} (if (util/model-ref? commodity)
                                            (models/find commodity :commodity)
                                            commodity)]
     (cond
       (every? nil? [earliest-price latest-price])
       (log/warnf
         "No price bounding for commodity %s %s"
         (:id commodity)
         (:commodity/symbol commodity))

       (and as-of
            (t/after? earliest-price as-of))
       (log/warnf
         "Unable to find %s price for commodity %s %s before first available date %s"
         as-of
         (:id commodity)
         (:commodity/symbol commodity)
         earliest-price)

       :else
       (models/find-by #:price{:commodity commodity
                               :trade-date [:between
                                            earliest-price
                                            (or as-of
                                                latest-price)]}
                       {:sort [[:price/trade-date :desc]]})))))

(defn rebound-commodity
  "Given a commodity, look up the earliest and latest prices and update the
  commodity record"
  [commodity]
  (let [criteria #:price{:commodity commodity
                         :trade-date [:between
                                      (settings/get :earliest-partition-date)
                                      (settings/get :latest-partition-date)]}
        earliest (models/find-by criteria {:sort [[:price/trade-date :asc]]})
        latest (models/find-by criteria {:sort [[:price/trade-date :desc]]})]
    (when latest
      (update-accounts latest))
    (when (and earliest latest)
      (models/put (assoc commodity
                         :commodity/earliest-price (:trade-date earliest)
                         :commodity/latest-price (:trade-date latest))))))

(defn- apply-to-account-chain
  [price]
  (fn [[{:as account :account/keys [value quantity]} & ancestors]]
    (let [new-value (* quantity price)
          adjustment (- new-value value)]
      (cons (assoc account :account/value new-value)
            (map #(update-in % [:account/value] + adjustment)
                 ancestors)))))

(defn- apply-to-account-chains
  [{:price/keys [commodity price]}]
  (mapcat (apply-to-account-chain price)
          (accounts/select-with-ancestors commodity)))

(defn- latest-price?
  [{:price/keys [commodity trade-date]}]
  (= 0
     (models/count #:price{:commodity commodity
                           :trade-date [:> trade-date]})))

(defmethod models/propagate :price
  [price]
  (cons price (when (latest-price? price)
                (apply-to-account-chains price))))

(defn- new-latest-price
  [{:price/keys [commodity trade-date] :keys [id]}]
  (let [price (models/find-by {:price/commodity commodity
                                      :id [:!= id]}
                                     {:sort [[:price/trade-date :desc]]})]
    (when (and price
               (t/before? (:price/trade-date price)
                          trade-date))
      price)))

(defmethod models/propagate-delete :price
  [price]
  (let [latest-price (new-latest-price price)]
    (cons price
          (when latest-price
            (apply-to-account-chains latest-price)))))
