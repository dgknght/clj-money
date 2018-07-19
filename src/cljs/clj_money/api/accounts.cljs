(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]))

(defn- after-read
  [account]
  (update-in account [:type] keyword))

(defn get-all
  [entity-id success-fn error-fn]
  (api/get-resources (api/path :entities entity-id :accounts)
                     #(success-fn (map after-read %))
                     error-fn))

(defn get-one
  [success-fn error-fn]
  (success-fn {:name "No a real account" :type :asset}))

(defn create
  [account success-fn error-fn]
  (error-fn "accounts api client is not implemented."))

(defn update
  [account success-fn error-fn]
  (error-fn "accounts api client is not implemented."))

(defn delete
  [account success-fn error-fn]
  (error-fn "accounts api client is not implemented."))
