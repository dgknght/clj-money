(ns clj-money.reports.commodities-account-summary
  (:require [java-time.api :as t]
            [clj-money.entities :as entities]
            [clj-money.util :as util]
            [clj-money.entities.transactions :as transactions]
            [clj-money.entities.prices :as prices]
            [clj-money.reports :refer [sum]]))

(defn- summarize-commodity
  [[commodity-id lots]]
  (let [commodity (entities/find commodity-id)
        shares (sum :lot/shares-owned lots)
        cost (->> lots
                  (map #(* (:lot/shares-owned %)
                           (:lot/purchase-price %)))
                  (reduce +))
        price (:price/value (prices/most-recent commodity))
        value (* price shares)
        gain (- value cost)]
    #:report{:caption (format "%s (%s)"
                              (:commodity/name commodity)
                              (:commodity/symbol commodity))
             :commodity (util/->entity-ref commodity)
             :style :data
             :depth 0
             :shares shares
             :price price
             :cost cost
             :value value
             :gain gain}))

(defn report
  ([account]
   (report account (t/local-date)))
  ([account as-of]
   (let [records (conj (->> (entities/select {:lot/account account
                                            :lot/shares-owned [:!= 0M]})
                            (group-by (comp :id :lot/commodity))
                            (map #(summarize-commodity %))
                            (sort-by :report/caption)
                            (into []))
                       #:report{:caption "Cash"
                                :style :data
                                :depth 0
                                :value (transactions/balance-as-of
                                         account
                                         as-of)})
         summary (reduce (fn [result record]
                           (reduce (fn [r k]
                                     (update-in r [k] #(+ % (or (k record) 0M))))
                                   result
                                   [:report/cost
                                    :report/value
                                    :report/gain]))
                         #:report{:caption "Total"
                                  :style :summary
                                  :cost 0M
                                  :value 0M
                                  :gain 0M}
                         records)]
     (conj records summary))))
