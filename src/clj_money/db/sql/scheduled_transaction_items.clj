(ns clj-money.db.sql.scheduled-transaction-items
  (:require [clj-money.db.sql :as sql]
            [clj-money.util :refer [temp-id?]]))

(defmethod sql/before-save :scheduled-transaction-item
  [item]
  (update-in item [:scheduled-transaction-item/action] name))

(defmethod sql/after-read :scheduled-transaction-item
  [item]
  (update-in item [:scheduled-transaction-item/action] keyword))

(defmethod sql/resolve-temp-ids :scheduled-transaction-item
  [item id-map]
  (update-in item
             [:scheduled-transaction-item/scheduled-transaction-id]
             (fn [id]
               (if (temp-id? id)
                 (id-map id)
                 id))))
