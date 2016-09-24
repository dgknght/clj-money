(ns clj-money.models.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [create-account
                                              select-accounts-by-entity-id]]))

(defn prepare-account-for-save
  "Adjusts account data for saving in the database"
  [account]
  ; convert account type from keyword to string
  (-> account
      (update-in [:type] name)
      (rename-keys {:entity-id :entity_id})))

(defn prepare-account-for-return
  "Adjusts account data read from the database for use"
  [account]
  (-> account
      (update-in [:type] keyword)
      (rename-keys {:entity_id :entity-id})))

(defn create
  "Creates a new account in the system"
  [storage-spec account]
  (->> account
       prepare-account-for-save
       (create-account (storage storage-spec))
       prepare-account-for-return))

(defn select-by-entity-id
  "Returns a list of all accounts in the system"
  [storage-spec entity-id]
  (->> storage-spec
       storage
       (select-accounts-by-entity-id entity-id)
       (map prepare-account-for-return)))
