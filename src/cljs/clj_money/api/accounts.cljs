(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [cljs-time.format :as f]
            [clj-money.api :as api]))

(defn- parse-local-date
  [string-date]
  (when string-date
    (f/parse-local (:date f/formatters) string-date)))

(defn- after-read
  [account]
  (-> account
      (update-in [:type] keyword)
      (update-in [:earliest-transaction-date] parse-local-date)
      (update-in [:latest-transaction-date] parse-local-date)))

(defn get-all
  [entity-id success-fn error-fn]
  (api/get-resources (api/path :entities entity-id :accounts)
                     #(success-fn (map after-read %))
                     error-fn))

(defn get-one
  [id success-fn error-fn]
  (api/get-resources (api/path :accounts id)
                     #(success-fn (after-read %))
                     error-fn))

(defn create
  [account success-fn error-fn]
  (api/create-resource (api/path :entities (:entity-id account) :accounts)
                       account
                       success-fn
                       error-fn))

(defn update
  [account success-fn error-fn]
  (api/update-resource (api/path :accounts (:id account))
                       account
                       success-fn
                       error-fn))

(defn delete
  [account success-fn error-fn]
  (api/delete-resource (api/path :accounts (:id account))
                       success-fn
                       error-fn))
