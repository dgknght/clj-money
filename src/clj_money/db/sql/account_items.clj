(ns clj-money.db.sql.account-items
  (:require [clojure.set :refer [rename-keys]]
            [clj-money.db.sql :as sql]))

(defmethod sql/before-save :account-item
  [item]
  (rename-keys item {:transaction/transaction-date :account-item/transaction-date}))

(defmethod sql/after-read :account-item
  [item]
  (-> item
      (update-in [:account-item/action] keyword)
      (rename-keys {:account-item/transaction-date :transaction/transaction-date})))
