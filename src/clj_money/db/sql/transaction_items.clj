(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :as types]))

(defmethod sql/resolve-temp-ids :transaction-item
  [item id-map]
  (types/resolve-temp-ids item id-map
    :transaction-item/transaction-id
    :transaction-item/account-id
    :transaction-item/reconciliation-id))

(defmethod sql/post-select :transaction-item
  [_ items]
  (map (fn [{:as item :transaction-item/keys [value]}]
         (cond-> (update-in-if item [:transaction/review-status] keyword)
           (nil? value)
           (dissoc :transaction-item/value)))
       items))
