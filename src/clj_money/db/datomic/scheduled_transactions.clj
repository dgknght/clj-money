(ns clj-money.db.datomic.scheduled-transactions
  (:require [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :scheduled-transaction
  [trx]
  (update-in trx [:scheduled-transaction/date-spec] pr-str))
