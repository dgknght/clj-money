(ns clj-money.models.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [clj-money.util :refer [rev-args]]
            [clj-money.models :as models]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [select-imports
                                              create-import
                                              update-import
                                              delete-import
                                              delete-image
                                              find-import-by-id]]
            [clj-money.models.entities :as entities]))

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

(defn- prepare-progress
  [progress]
  (-> progress
      (json/parse-string true)
      (select-keys [:account
                    :transaction
                    :budget
                    :commodity
                    :price
                    :finished
                    :error])))

(defn- entity-exists?
  [imp storage]
  (boolean
    (entities/find-by storage
                      {:user-id (:user-id imp)
                       :name (:entity-name imp)})))

(defn- after-read
  [imp storage]
  (when imp
    (-> imp
        (update-in [:progress] prepare-progress)
        (assoc :entity-exists? (entity-exists? imp storage))
        (models/tag ::models/import))))

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
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map #(after-read % s)
          (select-imports s criteria options)))))

(defn delete
  [storage-spec id]
  (with-transacted-storage [s storage-spec]
    (let [imp (find-by-id s id)]
      (doseq [image-id (:image-ids imp)]
        (delete-image s image-id))
      (delete-import s id))))
