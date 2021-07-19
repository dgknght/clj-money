(ns clj-money.models.accounts
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [stowaway.core :as stow :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.core :refer [assoc-if
                                          present?]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]))

(declare find-by find)

(defn- name-is-unique?
  [{:keys [id] :as account}]
  (nil? (find-by (-> account
                     (select-keys [:entity-id :parent-id :name :type])
                     (update-in [:parent-id] identity) ; Ensure that nil is included, as it matters for this query
                     (assoc-if :id (when id [:!= id]))))))
(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:name]})

(defn- parent-has-same-type?
  "Validation rule that ensure an account
  has the same type as its parent"
  [{:keys [parent-id type]}]
  (or (nil? parent-id)
      (= type
         (:type (find parent-id)))))
(v/reg-spec parent-has-same-type? {:message "%s must match the parent type"
                                   :path [:type]})

(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::name (s/and string?
                     present?))
(s/def ::type #{:asset :liability :equity :income :expense})
(s/def ::commodity-id integer?)
(s/def ::parent-id (s/nilable integer?))
(s/def ::system-tags (s/coll-of keyword? :kind set?))
(s/def ::user-tags (s/coll-of keyword? :kind set?))
(s/def ::new-account (s/and (s/keys :req-un [::entity-id ::name ::type ::commodity-id]
                                    :opt-un [::parent-id ::system-tags ::user-tags])
                            name-is-unique?
                            parent-has-same-type?))
(s/def ::existing-account (s/and (s/keys :req-un [::id ::entity-id ::type ::name]
                                         :opt-un [::parent-id ::commodity-id ::system-tags ::user-tags])
                                 name-is-unique?
                                 parent-has-same-type?))
; :value and :children-value are not specified because they are always
; calculated and not passed in

(defn- default-commodity-id
  [entity-id]
  (let [entity (entities/find entity-id)]
    (or (-> entity :settings :default-commodity-id)
        (->> {:entity-id entity-id
              :type :currency}
             commodities/search
             first
             :id))))

(declare find)
(defn- before-validation
  "Adjust account data for validation"
  [account]
  (cond-> account
    ; If no entity is specified, try to look it up
    (and (:id account)
         (nil? (:entity-id account)))
    (assoc :entity-id (:entity-id (find (:id account))))

    ; strip out empty string for parent-id
    (and (string? (:parent-id account))
         (empty? (:parent-id account)))
    (dissoc :parent-id)

    ; if no commodity is specified, use the default
    (nil? (:commodity-id account))
    (assoc :commodity-id (default-commodity-id (:entity-id account)))))

(defn- before-save
  "Adjusts account data for saving in the database"
  [account & _]
  (-> account
      (tag ::models/account)
      (update-in [:quantity] (fnil identity 0M))
      (update-in [:value] (fnil identity 0M))
      (update-in [:type] name)
      (update-in [:user-tags] #(when (seq %)
                                 (into-array (map name %))))
      (update-in [:system-tags] #(when (seq %)
                                   (into-array (map name %))))
      (dissoc :commodity)))

(defn- dissoc-if-nil
  "Removes the key from the map if the value is nil"
  [m k]
  (if (and (contains? m k)
           (nil? (get-in m [k])))
    (dissoc m k)
    m))

(defn- prepare-tags
  [tags]
  (->> tags
       (map keyword)
       set))

(defn- after-read
  "Adjusts account data read from the database for use"
  [account & _]
  (-> account
      (update-in [:type] keyword)
      (update-in [:system-tags] prepare-tags)
      (update-in [:user-tags] prepare-tags)
      (tag ::models/account)
      (dissoc-if-nil :parent-id)))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/account)
                          options)))))

(defn find-by
  "Returns the first account that matches the specified criteria"
  [criteria]
  (first (search criteria {:limit 1})))

(defn find
  "Returns the account having the specified id"
  [id-or-account]
  (when id-or-account (find-by {:id (->id id-or-account)})))

(defn find-by-name
  "Returns the account having the specified name"
  [entity-id account-name]
  (find-by {:entity-id entity-id
            :name account-name}))

(defn create
  [account]
  (with-storage (env :db)
    (let [account (before-validation account)]
      (with-validation account ::new-account
        (-> account
            before-save
            storage/create
            after-read)))))

(defn reload
  "Returns a fresh copy of the specified account from the data store"
  [model-or-id]
  (find (->id model-or-id)))

(defn update
  [account]
  (with-storage (env :db)
    (with-validation account ::existing-account
      (-> account
          before-save
          storage/update)
      (find account))))

(defn delete
  "Removes the account from the system"
  [account]
  (with-storage (env :db)
    (storage/delete account)))
