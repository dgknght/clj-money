(ns clj-money.db.datomic.cached-prices
  (:require [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :cached-price
  [price]
  (update-in price [:cached-price/trade-date] ->local-date))
