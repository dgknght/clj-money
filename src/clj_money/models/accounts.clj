(ns clj-money.models.accounts
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [config.core :refer [env]]
            [stowaway.core :as stow :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.core :refer [assoc-if
                                          parse-int
                                          update-in-if]]
            [dgknght.app-lib.models :refer [->id
                                            extract-nested]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.util :refer [live-id]]
            [clj-money.models :as models]))

(declare find-by find)

(defn- name-is-unique?
  [account]
  (= 0 (models/count (-> account
                         (select-keys [:account/entity
                                       :account/parent
                                       :account/name
                                       :account/type])
                         (update-in [:account/parent] identity) ; Ensure that nil is included, as it matters for this query
                         (assoc-if :id (when-let [id (live-id account)]
                                         [:!= id]))))))
(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:account/name]})

(defn- parent-has-same-type?
  "Validation rule that ensure an account
  has the same type as its parent"
  [{:account/keys [parent type]}]
  (or (nil? parent)
      (= type
         (:account/type (models/find parent)))))
(v/reg-spec parent-has-same-type? {:message "%s must match the parent type"
                                   :path [:account/type]})

(s/def :account/entity ::models/model-ref)
(s/def :account/name string?)
(s/def :account/type #{:asset :liability :equity :income :expense})
(s/def :account/commodity ::models/model-ref)
(s/def :account/parent (s/nilable ::models/model-ref))
(s/def :account/system-tags (s/nilable (s/coll-of keyword? :kind set?)))
(s/def :account/user-tags (s/nilable (s/coll-of keyword? :kind set?)))
(s/def :account/allocations (s/nilable (s/map-of integer? decimal?)))

(s/def ::models/account (s/and (s/keys :req [:account/entity
                                             :account/type
                                             :account/name
                                             :account/commodity]
                                         :opt [:account/parent
                                               :account/system-tags
                                               :account/user-tags
                                               :account/allocations])
                                 name-is-unique?
                                 parent-has-same-type?))
; :value and :children-value are not specified because they are always
; calculated and not passed in

(defn- default-commodity
  [entity]
  (if-let [ref (get-in entity [:entity/settings :settings/default-commodity])]
    (models/find (:id ref) :commodity)
    (models/find-by {:commodity/entity entity
                     :commodity/type :currency})))

(defn- resolve-entity
  [{:account/keys [entity] :as account}]
  (if entity
    account
    (assoc account :account/entity (models/find (:id entity) :entity))))

(defn- append-default-commodity
  [{:as account :account/keys [entity]}]
  (assoc account :account/commodity (default-commodity entity)))

(defmethod models/before-validation :account
  [{:as account :account/keys [commodity]}]
  (if commodity
    account
    (-> account resolve-entity append-default-commodity)))

(declare find)
#_(defn- before-validation
  "Adjust account data for validation"
  [account]
  (cond-> account
    ; If no entity is specified, try to look it up
    (and (:id account)
         (nil? (:entity-id account)))
    (assoc :entity-id (:entity-id (models/find (:id account) :account)))

    ; strip out empty string for parent-id
    (and (string? (:parent-id account))
         (empty? (:parent-id account)))
    (dissoc :parent-id)

    ; if no commodity is specified, use the default
    (nil? (:commodity-id account))
    (assoc :commodity-id (default-commodity-id (:entity-id account)))))

(defmethod models/before-save :account
  [account]
  (-> account
      (update-in [:account/quantity] (fnil identity 0M))
      (update-in [:account/value] (fnil identity 0M))))

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

(defn- deserialize-allocations
  [allocations]
  (->> allocations
       (map #(-> %
                 (update-in [0] parse-int)
                 (update-in [1] bigdec)))
       (into {})))

(defn- after-read
  [account]
  (-> account
      (update-in [:type] keyword)
      (update-in [:system-tags] prepare-tags)
      (update-in [:user-tags] prepare-tags)
      (update-in-if [:allocations] deserialize-allocations)
      (extract-nested :commodity)
      (tag ::models/account)
      (dissoc-if-nil :parent-id)))

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/account)
                          options)))))

(defn ^:deprecated find-by
  "Returns the first account that matches the specified criteria"
  [criteria]
  (first (models/select criteria {:limit 1})))

(defn ^:deprecated find
  "Returns the account having the specified id"
  [id-or-account]
  (when id-or-account (models/find (->id id-or-account) :account)))

(defn ^:deprecated create
  [account]
  (with-storage (env :db)
    (with-validation account ::new-account
      (-> account
          before-save
          storage/create
          after-read))))

(defn ^:deprecated reload
  "Returns a fresh copy of the specified account from the data store"
  [model-or-id]
  (models/find (->id model-or-id) :account))

(defn ^:deprecated update
  [account]
  (with-storage (env :db)
    (with-validation account ::existing-account
      (-> account
          before-save
          storage/update)
      (models/find account))))

(defn ^:deprecated delete
  "Removes the account from the system"
  [account]
  (with-storage (env :db)
    (storage/delete account)))
