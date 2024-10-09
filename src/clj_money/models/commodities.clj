(ns clj-money.models.commodities
  (:refer-clojure :exclude [update count find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-ex-validation]]
            [clj-money.db :as db]
            [clj-money.models.sql-storage-ref]))

(declare find-by)

(defn- name-is-unique?
  [{:keys [id] :as commodity}]
  (nil?
    (find-by
      (-> commodity
          (select-keys [:commodity/name
                        :commodity/exchange
                        :commodity/entity])
          (assoc-if :id (when id [:!= id]))))))

(v/reg-spec name-is-unique? {:message "%s is already in use"
                             :path [:commodity/name]})

(defn- symbol-is-unique?
  [{:keys [id] :as commodity}]
  (nil?
    (find-by
      (-> commodity
          (select-keys [:commodity/symbol
                        :commodity/exchange
                        :commodity/entity])
          (assoc-if :id (when id [:!= id]))))))

(v/reg-spec symbol-is-unique? {:message "%s is already in use"
                               :path [:commodity/symbol]})

(s/def :commodity/entity map?)
(s/def :commodity/name string?)
(s/def :commodity/symbol string?)
(s/def :commodity/type #{:currency :stock :fund})
(s/def :commodity/exchange (s/nilable #{:nasdaq :nyse :otc}))

(s/def :price-config/enabled boolean?)
(s/def :commodity/price-config (s/keys :req [:price-config/enabled]))

(defmulti commodity-base :commodity/type)
(defmethod commodity-base :currency [_]
  (s/keys :req []))
(defmethod commodity-base :default [_]
  (s/keys :req [:commodity/exchange]))

(s/def ::commodity (s/and (s/merge (s/multi-spec commodity-base :commodity/type)
                                   (s/keys :req [:commodity/type
                                                 :commodity/name
                                                 :commodity/symbol
                                                 :commodity/entity]
                                           :opt [:commodity/price-config]))
                              name-is-unique?
                              symbol-is-unique?))

(defn search
  "Returns commodities matching the specified criteria"
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (db/select (db/storage)
              (db/model-type criteria :commodity)
              options)))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (merge options {:limit 1})))))

(defn find
  "Returns the commodity having the specified ID"
  [id-or-commodity]
  (find-by {:id (->id id-or-commodity)}))

(defn- yield-or-find
  [m-or-id]
  ; if we have a map, assume it's a model and return it
  ; if we don't, assume it's an ID and look it up
  (if (map? m-or-id)
    m-or-id
    (find m-or-id)))

(defn- resolve-put-result
  [records]
  (some yield-or-find records)) ; This is because when adding a user, identities are inserted first, so the primary record isn't the first one returned

(defn put
  [commodity]
  (with-ex-validation commodity ::commodity
    (let [records-or-ids (db/put (db/storage)
                                 [commodity])]
      (resolve-put-result records-or-ids))))

(defn count
  "Returns the number of commodities matching the specified criteria"
  [criteria]
  (:record-count (search criteria {:count true})))

(defn delete
  "Removes a commodity from the system"
  [commodity]
  (db/delete (db/storage) [commodity]))
