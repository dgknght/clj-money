(ns clj-money.models.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core
             :as storage
             :refer [with-storage
                     with-transacted-storage]]
            [clj-money.util :refer [rev-args]]
            [clj-money.models :as models]
            [clj-money.models.helpers :refer [create-fn]]
            [clj-money.models.images :as images])
  (:import org.joda.time.LocalDate))

(s/def ::transaction-id uuid?)
(s/def ::transaction-date #(instance? LocalDate %))
(s/def ::image-id integer?)
(s/def ::caption string?)
(s/def ::new-attachment (s/keys :req-un [::transaction-id
                                         ::transaction-date
                                         ::caption
                                         ::image-id]))

(defn- after-read
  [attachment & _]
  (when attachment
    (storage/tag attachment ::models/attachment)))

(defn- before-save
  [attachment & _]
  (storage/tag attachment ::models/attachment))

(def create
  (create-fn {:create (rev-args storage/create)
              :before-save before-save
              :after-read after-read
              :spec ::new-attachment}))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read
          (storage/select s
                          (storage/tag criteria ::models/attachment)
                          options)))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (assoc options :limit 1)))))

(defn find-by-id
  [storage-spec id]
  (find-by storage-spec {:id id}))

(defn delete
  [storage-spec id-or-attachment]
  (with-transacted-storage [s storage-spec]
    (let [attachment (if (integer? id-or-attachment)
                       (find-by-id s id-or-attachment)
                       id-or-attachment)
          image (images/find-by s {:id (:image-id attachment)})]
      (images/delete s image)
      (storage/delete s attachment))))
