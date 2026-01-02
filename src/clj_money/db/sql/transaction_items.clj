(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.util :refer [temp-id?]]))

(defmethod sql/resolve-temp-ids :transaction-item
  [{:transaction-item/keys [transaction-id
                            reconciliation-id
                            account-id]
    :as item}
   id-map]
  (cond-> item
    (temp-id? transaction-id)    (update-in [:transaction-item/transaction-id] id-map)
    (temp-id? reconciliation-id) (update-in [:transaction-item/reconciliation-id] id-map)
    (temp-id? account-id)        (update-in [:transaction-item/account-id] id-map)))

(defmethod sql/before-save :transaction-item
  [item]
  (-> item
      (update-in-if [:transaction-item/action] name)
      (update-in [:transaction-item/index] (fnil identity (rand-int 1000)))))

(defmethod sql/after-read :transaction-item
  [item]
  (-> item
      (rename-keys {:transaction-item/transaction-date :transaction/transaction-date})
      (update-in-if [:transaction/transaction-date] t/local-date)))
