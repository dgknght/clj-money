(ns clj-money.db.sql.lot-items
  (:require [clj-money.db.sql :as sql]
            [clj-money.util :refer [temp-id?]]))

(defmethod sql/after-read :lot-item
  [trx]
  (update-in trx [:lot-item/action] keyword))

(defmethod sql/resolve-temp-ids :lot-item
  [{:as item :lot-item/keys [transaction-id lot-id]} id-map]
  (cond-> item
    (temp-id? transaction-id) (update-in [:lot-item/transaction-id] id-map)
    (temp-id? lot-id)         (update-in [:lot-item/lot-id] id-map)))
