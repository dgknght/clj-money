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
                                              delete-account
                                              account-exists-with-name?]]))

(def types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(def NewAccount
  {:entity-id s/Int
   :name s/Str
   :type (s/enum :asset :liability :equity :income :expense)
   (s/optional-key :parent-id) s/Int})

(def Account
  {:id s/Int
   :entity-id s/Int
   (s/optional-key :name) s/Str
   (s/optional-key :type) (s/enum :asset :liability :equity :income :expense)
   (s/optional-key :parent-id) s/Int})

(declare find-by-id)
(defn- before-update-validation
  "Adjust account data for validation"
  [storage account]
  (cond-> account
    (nil? (:entity-id account))
    (assoc :entity-id (:entity-id (find-by-id storage (Integer. (:id account)))))))

(defn- prepare-for-save
  "Adjusts account data for saving in the database"
  [storage account]
  ; convert account type from keyword to string
  (cond-> account ; TODO should coercion handle this?
    (:type account) (update-in [:type] name)))

(defn- prepare-for-return
  "Adjusts account data read from the database for use"
  [account]
  (cond-> account

    ; Remove :parent-id if it's nil
    (and
      (contains? account :parent-id)
      (nil? (:parent-id account)))
    (dissoc :parent-id)

    ; :type should already be present
    ; and should be a keyword
    true
    (update-in [:type] keyword)))

(defn- name-must-be-unique
  "Validation rule function that ensures an account
  name is unique within an entity"
  [storage {account-name :name entity-id :entity-id :as model}]
  {:model model
   :errors (if (and account-name
                    entity-id
                    (account-exists-with-name? storage entity-id account-name))
             [[:name "Name is already in use"]]
             [])})

(defn- must-have-same-type-as-parent
  "Validation rule that ensure an account
  has the same type as its parent"
  [storage {:keys [parent-id type] :as model}]
  {:model model
   :errors (if (and type
                    parent-id
                    (let [parent (find-by-id storage parent-id)]
                      (not (= type
                              (:type parent)))))
             [[:type "Type must match the parent type"]]
             [])})

(defn- validation-rules
  "Returns the account validation rules"
  [storage schema]
  [(partial validation/apply-schema schema)
   (partial name-must-be-unique storage)
   (partial must-have-same-type-as-parent storage)])

(defn- validate-new-account
  [storage account]
  (validation/validate-model account (validation-rules storage NewAccount)))

(defn- validate-account
  [storage account]
  (validation/validate-model account (validation-rules storage Account)))

(defn create
  "Creates a new account in the system"
  [storage-spec account]
  (let [storage (storage storage-spec)
        validated (validate-new-account storage account)]
    (if (validation/has-error? validated)
      validated
      (->> validated
           (partial prepare-for-save storage)
           (create-account storage)
           prepare-for-return))))

(defn find-by-id
  "Returns the account having the specified id"
  [storage-spec id]
  (prepare-for-return
    (find-account-by-id (storage storage-spec) id)))

(defn select-by-entity-id
  "Returns a list of all accounts in the system"
  [storage-spec entity-id]
  (map prepare-for-return
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
        validated (->> account
                       (before-update-validation st)
                       (validate-account st))]
    (if (validation/has-error? validated)
      validated
      (do
        (update-account st (prepare-for-save validated))
        (prepare-for-return (find-by-id st (:id validated)))))))

(defn delete
  "Removes the account from the system"
  [storage-spec id]
  (delete-account (storage storage-spec) id))
