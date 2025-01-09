(ns clj-money.models.accounts
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [stowaway.core :as stow :refer [tag]]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :refer [live-id]]
            [clj-money.models :as models]))

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
         (:account/type (models/find parent :account)))))
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
    (models/find ref :commodity)
    (models/find-by #:commodity{:entity entity
                                :type :currency})))

(defn- ensure-commodity
  [{:as account :account/keys [entity]}]
  (update-in account
             [:account/commodity]
             #(or %
                  (default-commodity entity))))

(defmethod models/before-validation :account
  [{:as account :account/keys [commodity]}]
  (if commodity
    account
    (-> account
        (update-in [:account/entity] (models/resolve-ref :entity))
        ensure-commodity)))

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
