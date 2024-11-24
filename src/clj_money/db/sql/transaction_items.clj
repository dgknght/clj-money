(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id?]]))

(defmethod sql/resolve-temp-ids :transaction-item
  [{:transaction-item/keys [transaction-id reconciliation-id] :as item} id-map]
  (cond-> item
    (temp-id? transaction-id)    (update-in [:transaction-item/transaction-id] id-map)
    (temp-id? reconciliation-id) (update-in [:transaction-item/reconciliation-id] id-map)))

(defmethod sql/before-save :transaction-item
  [item]
  (update-in-if item [:transaction-item/action] name))

(defmethod sql/after-read :transaction-item
  [item]
  (-> item
      (update-in [:transaction-item/action] keyword)
      (update-in-if [:transaction-item/transaction-date] t/local-date)))
