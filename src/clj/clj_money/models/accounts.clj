(ns clj-money.models.accounts
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.util :refer [assoc-if
                                    ->id]]
            [clj-money.validation :as v :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]))

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name v/non-empty-string?)
(s/def ::type #{:asset :liability :equity :income :expense})
(s/def ::commodity-id integer?)
(s/def ::parent-id (s/nilable integer?))
(s/def ::new-account (s/keys :req-un [::entity-id ::name ::type ::commodity-id]
                             :opt-un [::parent-id]))
(s/def ::existing-account (s/keys :req-un [::id ::entity-id ::type ::name]
                                  :opt-un [::parent-id ::commodity-id]))
; :value and :children-value are not specified because they are always
; calculated and not passed in

(defn- default-commodity-id
  [storage entity-id]
  (let [entity (entities/find-by-id storage entity-id)]
    (or (-> entity :settings :default-commodity-id)
        (->> {:entity-id entity-id
              :type :currency}
             (commodities/search storage)
             first
             :id))))

(declare find-by-id)
(defn- before-validation
  "Adjust account data for validation"
  [account storage]
  (cond-> account
    ; If no entity is specified, try to look it up
    (and (:id account)
         (nil? (:entity-id account)))
    (assoc :entity-id (:entity-id (find-by-id (:id account))))

    ; strip out empty string for parent-id
    (and (string? (:parent-id account))
         (empty? (:parent-id account)))
    (dissoc :parent-id)

    ; if no commodity is specified, use the default
    (nil? (:commodity-id account))
    (assoc :commodity-id (default-commodity-id storage (:entity-id account)))))

(defn- before-save
  "Adjusts account data for saving in the database"
  [account & _]
  (-> account
      (storage/tag ::models/account)
      (update-in [:quantity] (fnil identity 0M))
      (update-in [:value] (fnil identity 0M))
      (update-in [:type] name)
      (update-in [:tags] #(if (seq %)
                            (into-array (map name %))
                            nil))
      (dissoc :commodity)))

(defn- dissoc-if-nil
  "Removes the key from the map if the value is nil"
  [m k]
  (if (and (contains? m k)
           (nil? (get-in m [k])))
    (dissoc m k)
    m))

(defn- after-read
  "Adjusts account data read from the database for use"
  [account & _]
  (-> account
      (update-in [:type] keyword)
      (update-in [:tags] #(->> %
                               (map keyword)
                               set))
      (storage/tag ::models/account)
      (dissoc-if-nil :parent-id)))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage [s (env :db)]
     (map after-read
          (storage/select s
                          (storage/tag criteria ::models/account)
                          options)))))

(defn find-by
  "Returns the first account that matches the specified criteria"
  [criteria]
  (first (search criteria {:limit 1})))

(defn find-by-id
  "Returns the account having the specified id"
  [id]
  (when id (find-by {:id id})))

(defn find-by-name
  "Returns the account having the specified name"
  [entity-id account-name]
  (find-by {:entity-id entity-id
            :name account-name}))


(defn- name-is-unique?
  [{:keys [id parent-id name entity-id type]}]
  (nil? (find-by (assoc-if {:entity-id entity-id
                                    :parent-id parent-id
                                    :name name
                                    :type type}
                                   :id [:!= id]))))

(defn- parent-has-same-type?
  "Validation rule that ensure an account
  has the same type as its parent"
  [{:keys [parent-id type]}]
  (or (nil? parent-id)
      (= type
         (:type (find-by-id parent-id)))))

(def ^:private validation-rules
  [(v/create-rule name-is-unique?
                  [:name]
                  "Name is already in use")
   (v/create-rule parent-has-same-type?
                  [:type]
                  "Type must match the parent type")])

(defn create
  [account]
  (with-storage [s (env :db)]
    (let [account (before-validation account s)]
      (with-validation account ::new-account validation-rules
        (as-> account a
          (before-save a)
          (storage/create s a)
          (after-read a))))))

(defn reload
  "Returns a fresh copy of the specified account from the data store"
  [model-or-id]
  (find-by-id (->id model-or-id)))

(defn update
  [account]
  (with-storage [s (env :db)]
    (with-validation account ::existing-account validation-rules
      (as-> account a
        (before-save a)
        (storage/update s a))
      (find-by-id (:id account)))))

(defn delete
  "Removes the account from the system"
  [account]
  (with-storage [s (env :db)]
    (storage/delete s account)))
