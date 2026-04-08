(ns clj-money.db.sql.lot-items
  (:require [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types]))

(defmethod sql/resolve-temp-ids :lot-item
  [item id-map]
  (types/resolve-temp-ids
    item
    id-map
    :lot-item/transaction-id
    :lot-item/lot-id))
