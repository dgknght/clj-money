(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]))

(defn search
  [account-id criteria success-fn error-fn]
  (api/get-resources (api/path :accounts account-id :transaction-items)
                     criteria
                     success-fn
                     error-fn))
