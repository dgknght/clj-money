(ns clj-money.db.datomic.lots
  (:require [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :lot
  [lot]
  (update-in lot [:lot/purchase-date] ->local-date))
