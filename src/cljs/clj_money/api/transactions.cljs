(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [cljs-time.format :as f]
            [clj-money.api :as api]))

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
  (api/update-resource (api/path :transactions (:id transaction))
                       transaction
                       success-fn
                       error-fn))

(defn- after-item-read
  [item]
  (update-in item [:action] keyword))

(defn- after-read
  [transaction]
  (update-in transaction [:items] #(map after-item-read %)))

(defn get-one
  [id transaction-date success-fn error-fn]
  (api/get-resources (api/path :transactions
                               (f/unparse (:date f/formatters) transaction-date)
                               id)
                     #(success-fn (after-read %))
                     error-fn))
