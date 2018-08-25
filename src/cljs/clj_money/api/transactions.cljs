(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]))

(defn search
  [criteria success-fn error-fn]
  (api/get-resources (api/path :transactions)
                     criteria
                     success-fn
                     error-fn))
