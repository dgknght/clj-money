(ns clj-money.db.sql.transaction-items
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.db.sql :as sql]
            [clj-money.util :refer [temp-id?]]))

(defmethod sql/resolve-temp-ids :transaction-item
  [{:transaction-item/keys [account-id transaction-id reconciliation-id]
    :as item}
   id-map]
  (cond-> item
    (temp-id? transaction-id)
    (update-in [:transaction-item/transaction-id] id-map)

    (temp-id? account-id)
    (update-in [:transaction-item/account-id] id-map)

    (temp-id? reconciliation-id)
    (update-in [:transaction-item/reconciliation-id] id-map)))

(defmethod sql/post-select :transaction-item
  [_ items]
  (map (fn [{:as item :transaction-item/keys [value]}]
         (cond-> item
           (nil? value)
           (dissoc :transaction-item/value)))
       items))
