(ns clj-money.db.sql.account-items
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :account-item
  [item]
  (update-in item [:account-item/action] keyword))

(defmethod sql/resolve-temp-ids :account-item
  [item id-map]
  (update-in-if item [:account-item/reconciliation-id] id-map))
