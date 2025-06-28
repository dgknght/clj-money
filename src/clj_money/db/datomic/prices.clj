(ns clj-money.db.datomic.prices
  (:require [clj-money.dates :refer [->local-date]] [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :price
  [price]
  (update-in price [:price/trade-date] ->local-date))
