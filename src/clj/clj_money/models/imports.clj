(ns clj-money.models.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [stowaway.core :as storage :refer [with-storage with-transacted-storage]]
            [clj-money.util :refer [rev-args]]
            [clj-money.models :as models]
            [clj-money.models.helpers :refer [create-fn
                                              update-fn]]
            [clj-money.models.entities :as entities]
            [clj-money.models.images :as images]))

(s/def ::id integer?)
(s/def ::entity-name string?)
(s/def ::image-ids (s/coll-of integer?))
(s/def ::user-id integer?)
(s/def ::new-import (s/keys :req-un [::user-id ::entity-name ::image-ids]))
(s/def ::progress map?)
(s/def ::existing-import (s/keys :req-un [::id ::progress]))

(defn- before-save
  [imp & _]
  (models/tag imp :import))

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
        (models/tag :import))))

(def create
  (create-fn {:create (rev-args storage/create)
              :before-save before-save
              :after-read after-read
              :spec ::new-import}))

(defn- before-update
  [import & _]
  (update-in import [:progress] json/generate-string))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map #(after-read % s)
          (storage/select s
                          (models/tag criteria :import)
                          (merge {:sort [:created-at]} options))))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (assoc options :limit 1)))))

(defn find-by-id
  [storage-spec id]
  (find-by storage-spec {:id id}))

(def update
  (update-fn {:update (rev-args storage/update)
              :find find-by-id
              :before-save before-update
              :spec ::existing-import}))

(defn delete
  [storage-spec id]
  (with-transacted-storage [s storage-spec]
    (let [imp (find-by-id s id)]
      (->> (:image-ids imp)
           (map #(images/find-by s {:id %}))
           (filter identity)
           (map #(images/delete s %))
           doall)
      (storage/delete s imp))))
