(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [clj-money.util :refer [serialize-date
                                    unserialize-date]]
            [clj-money.api :as api]))

(defn- transaction-path
  [{:keys [id transaction-date]}]
  (api/path :transactions
            (serialize-date transaction-date)
            id))

(defn search
  [criteria success-fn error-fn]
  (api/get-resources (api/path :transactions)
                     criteria
                     success-fn
                     error-fn))

(defn- serialize
  [transaction]
  (update-in transaction [:transaction-date] serialize-date))

(defn create
  [transaction success-fn error-fn]
  (api/create-resource (api/path :entities
                                 (:entity-id transaction)
                                 :transactions)
                       (serialize transaction)
                       success-fn
                       error-fn))

(defn update
  [transaction success-fn error-fn]
  (api/update-resource (transaction-path transaction)
                       (serialize transaction)
                       success-fn
                       error-fn))

(defn save
  [transaction success-fn error-fn]
  (if (:id transaction)
    (update transaction success-fn error-fn)
    (create transaction success-fn error-fn)))

(defn- after-item-read
  [item]
  (update-in item [:action] keyword))

(defn- after-read
  [{:keys [transaction-date] :as transaction}]
  (let [transaction-date (unserialize-date transaction-date)]
    (-> transaction
        (assoc :original-transaction-date transaction-date
               :transaction-date transaction-date)
        (update-in [:items] #(map after-item-read %)))))

(defn get-one
  [tkey success-fn error-fn]
  (api/get-resources (transaction-path tkey)
                     (comp success-fn after-read)
                     error-fn))

(defn delete
  [transaction success-fn error-fn]
  (api/delete-resource (transaction-path transaction)
                       success-fn
                       error-fn))
