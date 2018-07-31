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
