(ns clj-money.db.sql.account-items
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [temp-id?]]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :account-item
  [item]
  (update-in item [:account-item/action] keyword))

(defmethod sql/resolve-temp-ids :account-item
  [{:as item :account-item/keys [reconciliation-id account-id]} id-map]
  (cond-> item
    (temp-id? reconciliation-id)
    (update-in [:account-item/reconciliation-id] id-map)

    (temp-id? account-id)
    (update-in [:account-item/account-id] id-map)))
