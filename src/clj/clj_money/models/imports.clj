(ns clj-money.models.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [stowaway.core :as storage :refer [with-storage
                                               with-transacted-storage]]
            [clj-money.util :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.validation :refer [with-validation]]
            [clj-money.models.entities :as entities]
            [clj-money.models.images :as images]))

(s/def ::id integer?)
(s/def ::entity-name string?)
(s/def ::image-ids (s/coll-of integer?))
(s/def ::user-id integer?)
(s/def ::lt-capital-gains-account string?)
(s/def ::st-capital-gains-account string?)
(s/def ::lt-capital-loss-account string?)
(s/def ::st-capital-loss-account string?)
(s/def ::options (s/keys :opt-un [::lt-capital-gains-account
                                  ::st-capital-gains-account
                                  ::lt-capital-loss-account
                                  ::st-captial-loss-account]))
(s/def ::new-import (s/keys :req-un [::user-id ::entity-name ::image-ids] :opt-un [::options]))
(s/def ::progress map?)
(s/def ::existing-import (s/keys :req-un [::id ::progress]))

(defn- before-save
  [imp]
  (-> imp
      (update-in-if [:options] pr-str)
      (storage/tag ::models/import)))

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
        (update-in-if [:options] read-string)
        (assoc :entity-exists? (entity-exists? imp storage))
        (storage/tag ::models/import))))

(defn create
  [storage impt]
  (with-storage [s storage]
    (with-validation impt ::new-import []
      (as-> impt i
        (before-save i)
        (storage/create s i)
        (after-read i s)))))

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
                          (storage/tag criteria ::models/import)
                          (merge {:sort [:created-at]} options))))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (assoc options :limit 1)))))

(defn find-by-id
  [storage-spec id]
  (find-by storage-spec {:id id}))

(defn update
  [storage impt]
  (with-storage [s storage]
    (with-validation impt ::existing-import []
      (storage/update s (before-update impt))
      (find-by-id s (:id impt)))))

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
