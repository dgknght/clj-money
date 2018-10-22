(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]))

(defn search
  [criteria success-fn error-fn]
  (api/get-resources (api/path :transactions)
                     criteria
                     success-fn
                     error-fn))

(defn create
  [transaction success-fn error-fn]

  (.log js/console "create " (prn-str transaction))
  
  #_(api/create-resource (api/path :entities
                                 (:entity-id transaction)
                                 :transactions)
                       success-fn
                       error-fn))
