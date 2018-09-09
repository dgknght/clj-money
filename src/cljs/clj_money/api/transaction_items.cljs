(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]
            [cljs-time.format :as f]))

(defn- after-read
  [item]
  (-> item
      (update-in [:transaction-date] #(f/parse-local (f/formatters :date) %))
      (update-in [:action] keyword)))

(defn search
  [account-id criteria success-fn error-fn]
  (api/get-resources (api/path :accounts account-id :transaction-items)
                                       criteria
                                       (fn [items]
                                         (success-fn (map after-read items)))
                                       error-fn))