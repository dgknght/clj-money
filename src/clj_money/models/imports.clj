(ns clj-money.models.imports
  (:refer-clojure :exclude [update find])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clj-money.db :as db]
            [clj-money.models :as models]))

(s/def :import/entity-name string?)
(s/def :import/images (s/coll-of ::models/model-ref :min-count 1))
(s/def :import/user ::models/model-ref)
(s/def :import/options (s/nilable (s/map-of keyword? string?)))
(s/def ::models/import (s/keys :req [:import/user
                                     :import/entity-name
                                     :import/images]
                               :opt [:import/options
                                     :import/progress]))

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

(defmethod models/propagate-delete :import
  [{:as imp :import/keys [images]}]
  (cons imp
        (map (comp (partial vector ::db/delete)
                   (db/model-type :image))
             images)))

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
