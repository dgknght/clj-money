(ns clj-money.db.sql.account-items
  (:require [clj-money.db.sql :as sql]))

(defmethod sql/after-read :account-item
  [item]
  (update-in item [:account-item/action] keyword))
