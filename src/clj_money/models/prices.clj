(ns clj-money.models.prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clj-money.config :refer [env]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]))

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
(s/def :price/value decimal?)
(s/def ::id uuid?)
(s/def ::models/price (s/and (s/keys :req [:price/commodity
                                           :price/trade-date
                                           :price/value]
                                     :opt [::id])
                             trade-date-unique?))

(defn most-recent
  ([commodity]
   (most-recent commodity nil))
  ([commodity as-of]
   {:pre [(map? commodity)]}

   (let [[earliest
          latest] (-> commodity
                      (models/resolve-ref :commodity)
                      (:commodity/price-date-range))]
     (cond
       (every? nil? [earliest latest])
       (do
         (log/warnf
           "No price bounding for commodity %s %s"
           (:id commodity)
           (:commodity/symbol commodity))
         (when (env :allow-unbounded-queries)
           (models/find-by #:price{:commodity commodity
                                   :trade-date [:<= as-of]}
                           {:sort [[:price/trade-date :desc]]})))

       (and as-of
            (t/after? earliest as-of))
       (log/warnf
         "Unable to find %s price for commodity %s %s before first available date %s"
         as-of
         (:id commodity)
         (:commodity/symbol commodity)
         earliest)

       :else
       (models/find-by #:price{:commodity commodity
                               :trade-date [:between
                                            earliest
                                            (or as-of
                                                latest)]}
                       {:sort [[:price/trade-date :desc]]})))))

(defn- apply-to-account
  [{:price/keys [value]}]
  (fn [{:as account :account/keys [quantity]}]
    (assoc account :account/value (* quantity value))))

(defn- apply-to-accounts
  [{:as price :price/keys [commodity]}]
  (map (apply-to-account price)
       (models/select {:account/commodity commodity})))

(defn- push-entity-bounds
  [{:price/keys [trade-date commodity]}]
  (-> (:commodity/entity commodity)
      (models/find :entity)
      (dates/push-model-boundary :entity/price-date-range trade-date)))

(defn- after-latest?
  [{:price/keys [trade-date commodity]}]
  (if-let [latest (last (:commodity/price-date-range commodity))]
    (t/before? latest trade-date)
    true))

(defn- push-commodity-boundaries
  [{:price/keys [commodity trade-date]}]
  (when (or (nil? (:commodity/price-date-range commodity))
            (dates/outside? trade-date (:commodity/price-date-range commodity)))
    (dates/push-model-boundary commodity :commodity/price-date-range trade-date)))

(defn- push-boundaries
  [price]
  (let [commodity (push-commodity-boundaries price)
        entity (push-entity-bounds price)
        accounts (when (after-latest? price)
                   (apply-to-accounts price))]
    (->> [commodity
          entity]
         (concat accounts)
         (filter identity))))

(defn- ->criteria
  [{:as commodity :commodity/keys [price-date-range]}]
  (cond-> {:price/commodity commodity}
    price-date-range (assoc :price/trade-date
                            (apply vector :between price-date-range))))

(defn- pull-boundaries
  [{:price/keys [trade-date]
    {[start end] :commodity/price-date-range
     :as commodity} :price/commodity}]
  (cond
    (= start end trade-date)
    nil

    (= start trade-date)
    (assoc commodity
           :commodity/price-date-range
           (if-let [new-start (-> (->criteria commodity)
                                  (models/find-by {:sort [:price/trade-date]})
                                  :price/trade-date)]
             [new-start end]
             nil))

    (= end trade-date)
    (let [new-end (models/find-by (->criteria commodity)
                                  {:sort [[:price/trade-date :desc]]})]
      (cons (assoc commodity
                   :commodity/price-date-range
                   (if new-end
                     [start (:price/trade-date new-end)]
                     nil))
            (apply-to-accounts new-end)))))

(defmethod prop/propagate :price
  [[before after]]
  (if after
    (-> after
        (update-in [:price/commodity] (models/find :commodity))
        push-boundaries)
    (-> before
        (update-in [:price/commodity] (models/find :commodity))
        pull-boundaries)))

(defn- aggregate
  [prices]
  (reduce (fn [m {:price/keys [trade-date commodity] :as price}]
            (-> (cond-> m
                  (or (nil? (get-in m [commodity :latest]))
                      (t/after? (get-in m [commodity :latest]) trade-date))
                  (assoc-in [:commodities commodity :current] price))
                (update-in [:commodities commodity :date-range] dates/push-boundary trade-date)
                (update-in [:date-range] dates/push-boundary trade-date)))
          {}
          prices))

(defn apply-agg-to-entity
  [entity {:keys [date-range]}]
  (apply dates/push-model-boundary entity :entity/price-date-range date-range))

(defn apply-agg-to-commodities
  [agg]
  (mapcat (fn [[commodity {:keys [current date-range]}]]
            (-> (models/find commodity :commodity)
                (assoc :commodity/price-date-range date-range)
                (cons (apply-to-accounts current))))
          (:commodities agg)))

(defn propagate-all
  ([opts]
   (doseq [e (models/select (util/model-type {} :entity))]
     (propagate-all e opts)))
  ([entity _opts]
   (when-let [prices (seq
                       (models/select
                         (util/model-type {:commodity/entity entity}
                                          :price)))]
     (let [agg (aggregate prices)]
       (models/put-many (cons (apply-agg-to-entity entity agg)
                              (apply-agg-to-commodities agg)))))))

(prop/add-full-propagation propagate-all :priority 1)
