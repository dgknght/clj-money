(ns clj-money.db.sql.lot-items
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types]))

(defmethod sql/before-save :lot-item
  [lot-item]
  (update-in lot-item [:lot-item/action] name))

(defmethod sql/after-read :lot-item
  [lot-item]
  (update-in lot-item [:lot-item/action] keyword))

(defmethod sql/resolve-temp-ids :lot-item
  [item id-map]
  (types/resolve-temp-ids
    item
    id-map
    :lot-item/transaction-id
    :lot-item/lot-id))
