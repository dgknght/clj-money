(ns clj-money.db.sql.lot-transactions
  (:require [clj-money.db.sql :as sql]))

(defmethod sql/after-read :lot-transaction
  [trx]
  (update-in trx [:lot-transaction/action] keyword))
