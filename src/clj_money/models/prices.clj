(ns clj-money.models.prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]))

(defn- trade-date-unique?
  [{:keys [id] :as price}]
  (zero?
    (-> price
        (select-keys [:price/commodity
                      :price/trade-date])
        (assoc-if :id (when id [:!= id]))
        (util/model-type :price)
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

; TODO: Rethink this, because probably value should not include children
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

(defn- update-entity
  [{:price/keys [trade-date commodity]}]
  (-> (models/find (:commodity/entity commodity) :entity)
      (update-in [:entity/settings :settings/earliest-price-date] dates/earliest trade-date)
      (update-in [:entity/settings :settings/latest-price-date] dates/latest trade-date)))

(defn- update-commodity
  [{:price/keys [commodity trade-date]}]
  (-> commodity
      (update-in [:commodity/earliest-price] dates/earliest trade-date)
      (update-in [:commodity/latest-price] dates/latest trade-date)))

(defn- latest-price?
  [{:price/keys [commodity trade-date]}]
  (= 0
     (models/count #:price{:commodity commodity
                           :trade-date [:> trade-date]})))

(defn- new-latest-price
  [{:price/keys [commodity trade-date] :keys [id]}]
  (let [price (models/find-by {:price/commodity commodity
                               :id [:!= id]}
                              {:sort [[:price/trade-date :desc]]})]
    (when (and price
               (t/before? (:price/trade-date price)
                          trade-date))
      price)))

(defmethod models/propagate :price
  [[before after]]
  (when-let [latest (if after
                      (when (latest-price? after)
                        after)
                      (new-latest-price before))]
    ; TODO: contract the date boundaries on delete on update the moves the date
    (let [price (update-in latest [:price/commodity] (models/find :commodity))]
      (cons (update-commodity price)
            (cons (update-entity price)
                  (apply-to-account-chains latest))))))

(defn- aggregate
  [prices]
  (reduce (fn [m {:price/keys [trade-date commodity] :as price}]
            (-> (cond-> m
                  (or (nil? (get-in m [commodity :latest]))
                      (t/after? (get-in m [commodity :latest]) trade-date))
                  (assoc-in [:commodities commodity :current] price))
                (update-in [:commodities commodity :earliest] dates/earliest trade-date)
                (update-in [:commodities commodity :latest] dates/latest trade-date)
                (update-in [:earliest] dates/earliest trade-date)
                (update-in [:latest] dates/latest trade-date)))
          {}
          prices))

(defn apply-agg-to-entity
  [entity agg]
  (-> entity
      (assoc-in [:entity/settings :settings/earliest-price-date]
                (:earliest agg))
      (assoc-in [:entity/settings :settings/latest-price-date]
                (:latest agg))))

(defn apply-agg-to-commodities
  [agg]
  (mapcat (fn [[commodity {:keys [current earliest latest]}]]
            (cons (-> (models/find commodity :commodity)
                      (assoc :commodity/earliest-price earliest
                             :commodity/latest-price latest))
                  (apply-to-account-chains current)))
          (:commodities agg)))

(defn propagate-all
  ([]
   (doseq [e (models/select (util/model-type {} :entity))]
     (propagate-all e)))
  ([entity]
   (when-let [prices (seq
                       (models/select
                         (util/model-type {:commodity/entity entity}
                                          :price)))]
     (let [agg (aggregate prices)]
       (models/put-many (cons (apply-agg-to-entity entity agg)
                              (apply-agg-to-commodities agg)))))))

(models/add-full-propagation propagate-all)
