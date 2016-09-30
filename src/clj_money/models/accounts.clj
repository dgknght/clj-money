(ns clj-money.models.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [schema.core :as s]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [create-account
                                              find-account-by-id
                                              select-accounts-by-entity-id
                                              update-account
                                              delete-account]]))

(def types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(def NewAccount
  {:entity-id s/Int
   :name s/Str
   :type (s/enum :asset :liability :equity :income :expense)})

(def Account
  {:id s/Int
   (s/optional-key :entity-id) s/Int
   (s/optional-key :name) s/Str
   (s/optional-key :type) (s/enum :asset :liability :equity :income :expense)})

(defn prepare-account-for-save
  "Adjusts account data for saving in the database"
  [account]
  ; convert account type from keyword to string
  (update-in account [:type] name))

(defn prepare-account-for-return
  "Adjusts account data read from the database for use"
  [account]
  (update-in account [:type] keyword))

(defn- validation-rules
  "Returns the account validation rules"
  [storage schema]
  [(partial validation/apply-schema schema)])

(defn- validate-new-account
  [account]
  (validation/validate-model account (validation-rules nil NewAccount)))

(defn- validate-account
  [account]
  (validation/validate-model account (validation-rules nil Account)))

(defn create
  "Creates a new account in the system"
  [storage-spec account]
  (let [validated (validate-new-account account)]
    (if (validation/has-error? validated)
      validated
      (->> validated
           prepare-account-for-save
           (create-account (storage storage-spec))
           prepare-account-for-return))))

(defn find-by-id
  "Returns the account having the specified id"
  [storage-spec id]
  (prepare-account-for-return
    (find-account-by-id (storage storage-spec) id)))

(defn select-by-entity-id
  "Returns a list of all accounts in the system"
  [storage-spec entity-id]
  (map prepare-account-for-return
       (select-accounts-by-entity-id (storage storage-spec)
                                     entity-id)))

(defn group-by-type
  "Returns the accounts for the specified entity grouped by type"
  ([storage-spec entity-id]
   (group-by-type (select-by-entity-id storage-spec entity-id)))
  ([accounts]
   (->> types
        (map (fn [type]
               [type (->> accounts
                          (filter #(= (:type %) type))
                          (sort-by :name))])))))

(defn update
  "Updates the specified account"
  [storage-spec account]
  (let [st (storage storage-spec)
        validated (validate-account account)]
    (update-account st (prepare-account-for-save validated))
    (prepare-account-for-return (find-by-id st (:id validated)))))

(defn delete
  "Removes the account from the system"
  [storage-spec id]
  (delete-account (storage storage-spec) id))
