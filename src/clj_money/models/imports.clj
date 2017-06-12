(ns clj-money.models.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-import
                                              update-import
                                              find-import-by-id]])
  (:import [java.io ByteArrayOutputStream
                    InputStream]))

(s/def ::entity-name string?)
(s/def ::image-id integer?)
(s/def ::user-id integer?)
(s/def ::new-import (s/keys :req-un [::user-id ::entity-name ::image-id]))
(s/def ::existing-import (s/keys :req-un []))

(def create
  (create-fn {:create create-import
              :spec ::new-import}))

(defn- before-update
  [_ import]
  (update-in import [:record-counts] json/generate-string))

(defn- after-read
  [_ import]
  (update-in import [:record-counts] #(json/parse-string % true)))

(defn find-by-id
  [storage-spec id]
  (with-storage [s storage-spec]
    (after-read s (find-import-by-id s id))))

(def update
  (update-fn {:update update-import
              :find find-by-id
              :before-save before-update
              :spec ::existing-import}))

(defn update-progress
  [storage-spec import-id progress]

  (throw (RuntimeException. "Not implemented"))

  #_(with-storage [s storage-spec]
    (update-import-progress s import-id progress)))
