(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]
            [clj-money.util :refer [parse-date]]))

(defn- after-read
  [item]
  (-> item
      (update-in [:transaction-date] #(parse-date %))
      (update-in [:action] keyword)))

(defn search
  ([criteria success-fn error-fn]
   (search criteria {} success-fn error-fn))
  ([criteria options success-fn error-fn]
   (api/get-resources (api/path :transaction-items)
                      criteria
                      options
                      (fn [items]
                        (success-fn (map after-read items)))
                      error-fn)))
