(ns clj-money.db.datomic.commodities
  (:require [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :commodity
  [{:commodity/keys [price-config] :as commodity}]
  [(dissoc commodity :commodity/price-config)
   price-config])
