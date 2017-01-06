(ns clj-money.models.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.set :refer [rename-keys]]
            [clj-money.validation :as validation]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-account
                                              find-account-by-id
                                              find-account-by-entity-id-and-name
                                              select-accounts-by-name
                                              select-accounts-by-entity-id
                                              update-account
                                              delete-account]])
  (:import java.math.BigDecimal))

(def account-types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name validation/non-empty-string?)
(s/def ::type #{:asset :liability :equity :income :expense})
(s/def ::parent-id integer?)
(s/def ::new-account (s/keys :req-un [::entity-id ::name ::type] :opt-un [::parent-id]))
(s/def ::existing-account (s/keys :req-un [::id] :opt-un [::entity-id ::name ::type ::parent-id]))

(declare find-by-id)
(defn- before-validation
  "Adjust account data for validation"
  [storage account]
  (cond-> account

    ; If no entity is specified, try to look it up
    (and (:id account) (nil? (:entity-id account)))
    (assoc :entity-id (:entity-id (find-by-id storage
                                              (Integer. (:id account)))))

    ; make sure type is a keyword
    (string? (:type account))
    (update-in [:type] keyword)

    ; strip out empty string for parent-id
    (and (string? (:parent-id account))
         (empty? (:parent-id account)))
    (dissoc :parent-id)))

(defn- before-create
  "Adjust account data prior to creation"
  [storage account]
  (assoc account :balance (bigdec 0)))

(defn- before-save
  "Adjusts account data for saving in the database"
  [storage account]
  (cond-> account
    ; convert account type from keyword to string
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

(defn- name-is-unique?
  [storage {:keys [id parent-id name entity-id]}]
  (->> (select-accounts-by-name storage entity-id name)
       (remove #(= (:id %) id))
       (filter #(= (:parent-id %) parent-id))
       empty?))

(defn- parent-has-same-type?
  "Validation rule that ensure an account
  has the same type as its parent"
  [storage {:keys [parent-id type] :as model}]
  (or (nil? parent-id)
      (= type
         (:type (find-by-id storage parent-id)))))

(defn- validation-rules
  [storage]
  (map (fn [{:keys [path message val-fn]}]
         (validation/create-rule (partial val-fn storage)
                                 path
                                 message))
       [{:val-fn name-is-unique?
         :path [:name]
         :message "Name is already in use"}
        {:val-fn parent-has-same-type?
         :path [:type]
         :message "Type must match the parent type"}]))

(defn create
  "Creates a new account in the system"
  [storage-spec account]
  (with-storage [s storage-spec]
    (let [prepared (->> account
                        (before-validation s))
          validated (apply validation/validate
                           ::new-account
                           prepared
                           (validation-rules s))]
      (if (validation/has-error? validated)
        validated
        (->> validated
             (before-create s)
             (before-save s)
             (create-account s)
             prepare-for-return)))))

(defn find-by-id
  "Returns the account having the specified id"
  [storage-spec id]
  (when id
    (with-storage [s storage-spec]
      (prepare-for-return (find-account-by-id s id)))))

(defn find-by-name
  "Returns the account having the specified name"
  [storage-spec entity-id account-name]
  (with-storage [s storage-spec]
    (prepare-for-return
      (find-account-by-entity-id-and-name s
                                          entity-id
                                          account-name))))

(defn reload
  "Returns a fresh copy of the specified account from the data store"
  [storage-spec {:keys [id]}]
  (find-by-id storage-spec id))

(defn select-by-entity-id
  "Returns a list of all accounts in the system"
  ([storage-spec entity-id] (select-by-entity-id storage-spec entity-id {}))
  ([storage-spec entity-id options]
   (with-storage [s storage-spec]
     (let [types (or (:types options)
                     (set account-types))]
       (->> (select-accounts-by-entity-id s entity-id)
            (map prepare-for-return)
            (filter #(types (:type %))))))))

(defn- append-path
  [account parent]
  (assoc account :path (str (:path parent) "/" (:name account))))

(defn- append-children
  [account all-accounts]
  (let [children (->> all-accounts
                      (filter #(= (:id account) (:parent-id %)))
                      (map #(append-path % account))
                      (map #(append-children % all-accounts))
                      (sort-by :name)
                      vec)]
    (assoc account :children children
                   :children-balance (reduce #(+ %1 (:balance %2) (:children-balance %2))
                                             0
                                             children))))

(defn select-nested-by-entity-id
  "Returns the accounts for the entity with children nested under
  parents and parents grouped by type"
  ([storage-spec entity-id]
   (select-nested-by-entity-id storage-spec entity-id account-types))
  ([storage-spec entity-id types]
   (let [all (select-by-entity-id storage-spec entity-id)
         grouped (->> all
                      (remove :parent-id)
                      (map #(assoc % :path (:name %)))
                      (map #(append-children % all))
                      (group-by :type))]
     (map #(hash-map :type % :accounts (or
                                         (->> grouped
                                              %
                                              (sort-by :name)
                                              vec)
                                         []))
          types))))

(defn update
  "Updates the specified account"
  [storage-spec account]
  (with-storage [s storage-spec]
    (let [validated (->> account
                         (before-validation s)
                         (validation/validate ::existing-account))]
      (if (validation/has-error? validated)
        validated
        (do
          (->> validated
               (before-save s)
               (update-account s))
          (->> validated
               :id
               (find-by-id s)
               prepare-for-return))))))

(defn delete
  "Removes the account from the system"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-account s id)))

(defn left-side?
  "Returns truthy if the specified account is asset or expense, falsey if anything else"
  [account]
  (#{:asset :expense} (:type account)))

(defn polarize-amount
  "Adjusts the polarity of an amount as appropriate given
  a transaction item action and the type of the associated account"
  [transaction-item account]
  (let [polarizer (* (if (left-side? account) 1 -1)
                     (if (= :debit (:action transaction-item)) 1 -1))]
    (* (:amount transaction-item) polarizer)))
