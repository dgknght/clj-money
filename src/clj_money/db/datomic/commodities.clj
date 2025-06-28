(ns clj-money.db.datomic.commodities
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as datomic]
            [clj-money.dates :as dates]))

(defmethod datomic/deconstruct :commodity
  [{:commodity/keys [price-config] :as commodity}]
  [(dissoc commodity :commodity/price-config)
   price-config])

(defmethod datomic/after-read :commodity
  [commodity]
  (-> commodity
      (update-in-if [:commodity/price-date-range 0] dates/->local-date)
      (update-in-if [:commodity/price-date-range 1] dates/->local-date)))

(defmethod datomic/propagate-delete :commodity
  [commodity {:keys [api]}]
  (->> (datomic/query api {:query '[:find ?p
                                    :where [?p :price/commodity ?c]
                                    :in $ ?c]
                           :args [(:id commodity)]})
       (map (comp #(hash-map :id %)
                  first))
       (cons commodity)))
