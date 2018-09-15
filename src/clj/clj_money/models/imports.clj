(ns clj-money.models.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clj-money.util :refer [rev-args]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [select-imports
                                              create-import
                                              update-import
                                              find-import-by-id]]))

(s/def ::id integer?)
(s/def ::entity-name string?)
(s/def ::image-ids (s/coll-of integer?))
(s/def ::user-id integer?)
(s/def ::new-import (s/keys :req-un [::user-id ::entity-name ::image-ids]))
(s/def ::progress map?)
(s/def ::existing-import (s/keys :req-un [::id ::progress]))

(def create
  (create-fn {:create (rev-args create-import)
              :spec ::new-import}))

(defn- before-update
  [import & _]
  (update-in import [:progress] json/generate-string))

(defn- after-read
  [import & _]
  (update-in import
             [:progress]
             #(-> %
                  (json/parse-string true)
                  (select-keys [:account :transaction :budget :commodity :price]))))

(defn find-by-id
  [storage-spec id]
  (with-storage [s storage-spec]
    (after-read (find-import-by-id s id) s)))

(def update
  (update-fn {:update (rev-args update-import)
              :find find-by-id
              :before-save before-update
              :spec ::existing-import}))

(defn search
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (map #(after-read s %) (select-imports s criteria))))
