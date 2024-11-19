(ns clj-money.models.imports
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :refer [keywordize-keys]]
            [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.models.images :as images]))

(s/def :import/entity-name string?)
(s/def :import/images (s/coll-of ::models/model-ref :min-count 1))
(s/def :import/user ::models/model-ref)
(s/def :import/options (s/nilable (s/map-of keyword? string?)))
(s/def ::models/import (s/keys :req [:import/user
                                     :import/entity-name
                                     :import/images]
                               :opt [:import/options
                                     :import/progress]))

(defn- prepare-progress
  [progress]
  (-> progress
      keywordize-keys
      (select-keys [:account
                    :transaction
                    :scheduled-transaction
                    :budget
                    :commodity
                    :price
                    :account-balance
                    :process-reconciliation
                    :finished
                    :errors])))

(defmethod models/before-save :import
  [imp]
  (dissoc imp :import/entity-exists?))

(defn- entity-exists?
  [{:import/keys [user entity-name]}]
  (< 0 (models/count #:entity{:user user
                              :name entity-name})))

(defmethod models/after-read :import
  [imp _]
  (assoc imp :import/entity-exists? (entity-exists? imp)))

(defn ^:deprecated create
  [_impt]
  (throw (UnsupportedOperationException. "create is deprecated")))

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "search is deprecated"))))

(defn ^:deprecated find-by
  ([criteria]
   (find-by criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "find-by is deprecated"))))

(defn ^:deprecated find
  [_import-or-id]
  (throw (UnsupportedOperationException. "find is deprecated")))

(defn ^:deprecated update
  [_impt]
  (throw (UnsupportedOperationException. "update is deprecated")))

(defn ^:deprecated delete
  [_imp]
  (throw (UnsupportedOperationException. "delete is deprecated")))
