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
(defn get-one
  [id transaction-date success-fn error-fn]
  (api/get-resources (api/path :transactions
                               (f/unparse (:date f/formatters) transaction-date)
                               id)
                     success-fn
                     error-fn))
