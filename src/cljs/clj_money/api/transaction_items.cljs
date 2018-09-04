(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]
            [cljs-time.format :as f]))

(defn- after-read
  [item]
  (update-in item [:transaction-date] #(f/parse-local (f/formatters :date) %)))

(defn search
  [account-id criteria success-fn error-fn]
  (api/get-resources (api/path :accounts account-id :transaction-items)
                                       criteria
                                       (fn [items]
                                         (success-fn (map after-read items)))
                                       error-fn))
