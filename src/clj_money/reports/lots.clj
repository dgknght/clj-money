(ns clj-money.reports.lots
  (:require [clj-money.entities :as entities]
            [clj-money.entities.prices :as prices]
            [clj-money.reports :refer [sum]]))

(defn- valuate-lot
  [current-price]
  (fn [{:as lot :lot/keys [shares-owned purchase-price]}]
    (let [cost-basis (* purchase-price shares-owned)
          current-value (* current-price shares-owned)]
      (assoc lot
             :lot/cost-basis cost-basis
             :lot/current-value current-value
             :lot/gain (- current-value cost-basis)))))

(defn- valuate-lots
  [commodity lots]
  (let [current-price (:price/value (prices/most-recent commodity))
        adj-lots (map (valuate-lot current-price) lots)
        shares-purchased (sum :lot/shares-purchased adj-lots)
        shares-owned (sum :lot/shares-owned adj-lots)
        current-value (* shares-owned current-price)
        cost-basis (sum :lot/cost-basis adj-lots)]
    (assoc commodity
           :commodity/current-price current-price
           :commodity/current-value current-value
           :commodity/shares-purchased shares-purchased
           :commodity/shares-owned shares-owned
           :commodity/cost-basis cost-basis
           :commodity/gain (- current-value cost-basis)
           :commodity/lots adj-lots)))

(defn report
  ([account]
   (report account nil))
  ([account commodity]
   (->> (entities/select (cond-> {:lot/account account
                                :lot/shares-owned [:> 0]}
                         commodity
                         (assoc :lot/commodity commodity))
                       {:sort [[:lot/purchase-date :asc]]})
        (map #(assoc %
                     :lot/items
                     (entities/select
                       {:lot-item/lot %}
                       {:sort [[:transaction/transaction-date :asc]]
                        :select-also [:transaction/transaction-date]})))
        (group-by (comp :id :lot/commodity))
        (map (comp #(apply valuate-lots %)
                   #(update-in % [0] entities/find)))
        (sort-by :commodity/name))))
