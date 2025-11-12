(ns clj-money.entities.prices
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
            [clj-money.decimal :as d]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]))

(defn- trade-date-unique?
  [{:keys [id] :as price}]
  (zero?
    (-> price
        (select-keys [:price/commodity
                      :price/trade-date])
        (assoc-if :id (when id [:!= id]))
        (util/entity-type :price)
        entities/count)))
(v/reg-spec trade-date-unique? {:message "%s already exists"
                                :path [:price/trade-date]})

(s/def :price/commodity ::entities/entity-ref)
(s/def :price/trade-date t/local-date?)
(s/def :price/value decimal?)
(s/def ::id uuid?)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/price (s/and (s/keys :req [:price/commodity
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
                      entities/resolve-ref
                      (:commodity/price-date-range))]
     (cond
       (every? nil? [earliest latest])
       (do
         (log/warnf
           "No price bounding for commodity %s %s"
           (:id commodity)
           (:commodity/symbol commodity))
         (when (env :allow-unbounded-queries)
           (entities/find-by #:price{:commodity commodity
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
       (entities/find-by #:price{:commodity commodity
                               :trade-date [:between
                                            earliest
                                            (or as-of
                                                latest)]}
                       {:sort [[:price/trade-date :desc]]})))))

(defn- apply-to-account
  [{:price/keys [value trade-date]} {:keys [force]}]
  (fn [{:as account :account/keys [quantity price-as-of]}]
    (cond-> account
      (or (nil? price-as-of)
          force
          (t/before? price-as-of trade-date))
      (assoc
        :account/commodity-price value
        :account/price-as-of trade-date
        :account/value (d/* quantity value)))))

(defn- apply-to-accounts
  [{:as price :price/keys [commodity]} & {:as opts}]
  (map (apply-to-account price opts)
       (entities/select {:account/commodity commodity})))

(defn- push-entity-bounds
  [{:price/keys [trade-date commodity]}]
  (-> (:commodity/entity commodity)
      entities/find
      (dates/push-entity-boundary :entity/price-date-range trade-date)))

(defn- after-latest?
  [{:price/keys [trade-date commodity]}]
  (if-let [latest (last (:commodity/price-date-range commodity))]
    (t/before? latest trade-date)
    true))

(defn- push-commodity-boundaries
  [{:price/keys [commodity trade-date]}]
  (when (or (nil? (:commodity/price-date-range commodity))
            (dates/outside? trade-date (:commodity/price-date-range commodity)))
    (dates/push-entity-boundary commodity :commodity/price-date-range trade-date)))

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
                                  (entities/find-by {:sort [:price/trade-date]})
                                  :price/trade-date)]
             [new-start end]
             nil))

    (= end trade-date)
    (let [new-end (entities/find-by (->criteria commodity)
                                  {:sort [[:price/trade-date :desc]]})]
      (cons (assoc commodity
                   :commodity/price-date-range
                   (if new-end
                     [start (:price/trade-date new-end)]
                     nil))
            (apply-to-accounts new-end :force true)))))

(defmethod prop/propagate :price
  [[before after]]
  (if after
    (-> after
        (update-in [:price/commodity] (entities/find :commodity))
        push-boundaries)
    (-> before
        (update-in [:price/commodity] (entities/find :commodity))
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
  (apply dates/push-entity-boundary entity :entity/price-date-range date-range))

(defn apply-agg-to-commodities-and-accounts
  [agg]
  (mapcat (fn [[commodity {:keys [current date-range]}]]
            (-> (entities/find commodity)
                (assoc :commodity/price-date-range date-range)
                (cons (apply-to-accounts current))))
          (:commodities agg)))

(defn- fetch-prices
  [entity]
  (entities/select
    (util/entity-type {:commodity/entity entity}
                     :price)))

(defn propagate-all
  [entity _opts]

  (log/debugf "[propagation] start entity %s" (:entity/name entity))

  (let [result (or (when-let [prices (seq (fetch-prices entity))]
                     (let [agg (aggregate prices)]
                       (-> entity
                           (apply-agg-to-entity agg)
                           (cons (apply-agg-to-commodities-and-accounts agg))
                           entities/put-many
                           first)))
                   entity)]

    (log/infof "[propagation] finish entity %s"
               (:entity/name result))

    result))

(prop/add-full-propagation propagate-all :priority 1)
