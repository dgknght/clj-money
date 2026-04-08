(ns clj-money.db.sql.scheduled-transaction-items
  (:require [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types]))

(defmethod sql/resolve-temp-ids :scheduled-transaction-item
  [item id-map]
  (types/resolve-temp-ids
    item
    id-map
    :scheduled-transaction-item/scheduled-transaction-id
    :scheduled-transaction-item/account-id))
