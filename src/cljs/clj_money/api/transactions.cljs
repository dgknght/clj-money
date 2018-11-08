(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [cljs-time.core :as t]
            [cljs-time.format :as f]
            [clj-money.api :as api]))

(defmulti ^:private serialize-date
  (fn [d]
    (cond
      (string? d) :string
      (t/date? d) :date)))

(defmethod ^:private serialize-date :date
  [d]
  (f/unparse (:date f/formatters) d))

(defmethod ^:private serialize-date :string
  [d]
  d)

(defn- transaction-path
  [{:keys [id transaction-date]}]
  (api/path :transactions
            (serialize-date  transaction-date)
            id))

(defn search
  [criteria success-fn error-fn]
  (api/get-resources (api/path :transactions)
                     criteria
                     success-fn
                     error-fn))

(defn create
  [transaction success-fn error-fn]
  (api/create-resource (api/path :entities
                                 (:entity-id transaction)
                                 :transactions)
                       transaction
                       success-fn
                       error-fn))

(defn update
  [transaction success-fn error-fn]
  (api/update-resource (transaction-path transaction)
                       transaction
                       success-fn
                       error-fn))

(defn- after-item-read
  [item]
  (update-in item [:action] keyword))

(defn- after-read
  [transaction]
  (-> transaction
      (assoc :original-transaction-date (:transaction-date transaction))
      (update-in [:items] #(map after-item-read %))))

(defn get-one
  [tkey success-fn error-fn]
  (api/get-resources (transaction-path tkey)
                     #(success-fn (after-read %))
                     error-fn))

(defn delete
  [transaction success-fn error-fn]
  (api/delete-resource (transaction-path transaction)
                       success-fn
                       error-fn))
