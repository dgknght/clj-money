(ns clj-money.models.prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [config.core :refer [env]]
            [java-time.api :as t]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-transacted-storage]]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.dates :as dates]
            [clj-money.find-in-chunks :as ch]
            [clj-money.models :as models]
            [clj-money.models.settings :as settings]
            [clj-money.models.commodities :as commodities]))

(def ^:dynamic *skip-account-updates* false)

(declare find-by)

(defn- trade-date-unique?
  [{:keys [id] :as price}]
  (-> price
      (select-keys [:price/commodity
                    :price/trade-date])
      (assoc-if :id (when id [:!= id]))
      models/find-by
      nil?))
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

(defn ^:deprecated search [& _]
  (throw (UnsupportedOperationException. "search is deprecated")))

(defn ^:deprecated find-by [& _]
  (throw (UnsupportedOperationException. "find-by is deprecated")))

(defn ^:deprecated find [& _]
  (throw (UnsupportedOperationException. "find is deprecated")))

(defn- before-save
  [price]
  (tag price ::models/price))

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
      (update-in [:price/commodity] #(models/find (:id %) :commodity))
      update-entity
      update-commodity
      update-accounts))

(defn ^:deprecated create [& _]
 (throw (UnsupportedOperationException. "create is deprecated")))

(defn- update-meta-for-change
  [after before]
  (when-not (t/= (:trade-date after)
                 (:trade-date before))
    (update-accounts after {:price-as-of (:price/trade-date before)})
    (update-commodity after))
  after)

(def ^:private working-trade-date
  (some-fn #(-> %
                meta
                :original-values
                :trade-date)
           :trade-date))

(defn ^:deprecated update
  [price]
  (with-transacted-storage (env :db)
    (with-validation price ::existing-price
      (let [before (find-by {:id (:id price)
                             :trade-date (working-trade-date price)})]
        (-> price
            before-save
            (update-meta-for-change before)
            storage/update)
        (models/find price)))))

(defn- update-meta-with-previous
  [{:price/keys [commodity trade-date] :as price}]
  (some-> (models/find-by #:price{:commodity commodity
                                  :trade-date [:< trade-date]}
                          {:sort [[:price/trade-date :desc]]})
          (update-accounts {:price-as-of trade-date})
          update-commodity)
  price)

(defn ^:deprecated delete
  [price]
  (with-transacted-storage (env :db)
    (-> price
        update-meta-with-previous
        storage/delete)))

(defn most-recent
  ([commodity]
   (most-recent commodity nil))
  ([{:commodity/keys [earliest-price latest-price] :as commodity} as-of]
   {:pre [(map? commodity)]}

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
                     {:sort [[:price/trade-date :desc]]}))))

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

(defn batch-fetch
  ([commodity-ids] (batch-fetch commodity-ids {}))
  ([commodity-ids opts]
   (let [as-of (or (:as-of opts) (t/local-date))]
     (ch/find commodity-ids
              (merge
               {:start-date as-of
                :time-step (t/years 1)
                :fetch-fn #(models/select
                             #:price{:commodity %1
                                     :trade-date [:and
                                                  [:> (t/minus %2 (t/years 1))]
                                                  [:<= %2]]})
                :transform-fn :price
                :id-fn :commodity-id
                :find-one-fn (fn [prices]
                               (->> prices
                                    (filter #(or (= (:trade-date %) as-of)
                                                 (t/before? (:trade-date %) as-of)))
                                    (sort-by t/after?)
                                    first))}
               opts)))))
