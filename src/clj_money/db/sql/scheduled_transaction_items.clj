(ns clj-money.db.sql.scheduled-transaction-items
  (:require [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id?]]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :scheduled-transaction-item/account)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :scheduled-transaction-item/account)

(defmethod sql/before-save :scheduled-transaction-item
  [item]
  (-> item
      (update-in [:scheduled-transaction-item/action] name)
      ->sql-refs))

(defmethod sql/after-read :scheduled-transaction-item
  [item]
  (-> item
      (update-in [:scheduled-transaction-item/action] keyword)
      ->sql-refs))

(defmethod sql/resolve-temp-ids :transaction-item
  [item id-map]
  (update-in item
             [:scheduled-transaction-item/scheduled-transaction-id]
             (fn [id]
               (if (temp-id? id)
                 (id-map id)
                 id))))
