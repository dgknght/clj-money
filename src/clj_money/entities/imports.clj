(ns clj-money.entities.imports
  (:refer-clojure :exclude [update find])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]))

(s/def :import/entity-name string?)
(s/def :import/images (s/coll-of ::entities/entity-ref :min-count 1))
(s/def :import/user ::entities/entity-ref)
(s/def :import/options (s/nilable (s/map-of keyword? string?)))
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/import (s/keys :req [:import/user
                                     :import/entity-name
                                     :import/images]
                               :opt [:import/options
                                     :import/progress]))

(defmethod entities/before-save :import
  [imp]
  (dissoc imp :import/entity-exists?))

(defn- entity-exists?
  [{:import/keys [user entity-name]}]
  (< 0 (entities/count #:entity{:user user
                              :name entity-name})))

(defmethod entities/after-read :import
  [imp _]
  (assoc imp :import/entity-exists? (entity-exists? imp)))

(defmethod prop/propagate :import
  [[{:import/keys [images]} after]]
  (when-not after
    (map (comp (partial vector ::db/delete)
               (util/entity-type :image))
         images)))
