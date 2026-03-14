(ns clj-money.db.sql.scheduled-transaction-items
  (:require [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types]))

(defmethod sql/before-save :scheduled-transaction-item
  [item]
  (update-in item [:scheduled-transaction-item/action] name))

(defmethod sql/after-read :scheduled-transaction-item
  [item]
  (update-in item [:scheduled-transaction-item/action] keyword))

(defmethod sql/resolve-temp-ids :scheduled-transaction-item
  [item id-map]
  (types/resolve-temp-ids
    item
    id-map
    :scheduled-transaction-item/scheduled-transaction-id
    :scheduled-transaction-item/account-id))
