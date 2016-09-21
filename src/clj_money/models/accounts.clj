(ns clj-money.models.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [create-account
                                              select-accounts]]))

(defn prepare-account-for-save
  "Adjusts account data for saving in the database"
  [account]
  ; convert account type from keyword to string
  (update-in account [:type] name))

(defn prepare-account-for-return
  "Adjusts account data read from the database for use"
  [account]
  (update-in account [:type] keyword))

(defn create
  "Creates a new account in the system"
  [storage-spec account]
  (->> account
       prepare-account-for-save
       (create-account (storage storage-spec))
       prepare-account-for-return))

(defn select
  "Returns a list of all accounts in the system"
  [storage-spec]
  (->> storage-spec
       storage
       select-accounts
       (map prepare-account-for-return)))
