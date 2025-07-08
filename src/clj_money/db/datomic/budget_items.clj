(ns clj-money.db.datomic.budget-items
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defn- set-kw-ns
  [m n]
  (update-keys m (comp #(keyword n %)
                       name)))

(defn- strip-kw-ns
  [m]
  (update-keys m (comp keyword name)))

(defn- stash-spec-id
  [m]
  (-> m
      (vary-meta assoc :spec (get-in m [:budget-item/spec :id]))
      (update-in-if [:budget-item/spec] dissoc :id)))

(defn- restore-spec-id
  [m]
  (if-let [spec-id (-> m meta :spec)]
    (assoc-in m [:budget-item/spec :id] spec-id)
    m))

; TODO: Should we just use the same ns as in datomic?
(defmethod datomic/before-save :budget-item
  [budget-item]
  (-> budget-item
      (update-in-if [:budget-item/spec] set-kw-ns "budget-item-spec" )
      (update-in-if [:budget-item/spec]
                    update-in
                    [:id]
                    (fnil identity (util/temp-id)))
      restore-spec-id))

(defmethod datomic/after-read :budget-item
  [budget-item]
  (-> budget-item
      (update-in-if [:budget-item/spec] strip-kw-ns)
      stash-spec-id))
