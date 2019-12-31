(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [cljs-time.format :as f]
            [clj-money.api :as api]
            [clj-money.util :refer [parse-date]]))

(defn- after-read
  [item]
  (-> item
      (update-in [:transaction-date] parse-date)
      (update-in [:action] keyword)))

(defn- format-date
  [date]
  (f/unparse (:date f/formatters) date))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (update-in [:transaction-date 1] format-date)
      (update-in [:transaction-date 2] format-date)))

(defn search
  ([criteria success-fn error-fn]
   (search criteria {} success-fn error-fn))
  ([criteria options success-fn error-fn]
   (api/get-resources (api/path :transaction-items)
                      (prepare-criteria criteria)
                      options
                      #(success-fn (map after-read %))
                      error-fn)))
