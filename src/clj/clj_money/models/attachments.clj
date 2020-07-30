(ns clj-money.models.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core
             :as storage
             :refer [with-storage
                     with-transacted-storage]]
            [clj-money.models :as models]
            [clj-money.validation :refer [with-validation]]
            [clj-money.models.images :as images])
  (:import org.joda.time.LocalDate))

(s/def ::id integer?)
(s/def ::transaction-id uuid?)
(s/def ::transaction-date #(instance? LocalDate %))
(s/def ::image-id integer?)
(s/def ::caption string?)
(s/def ::new-attachment (s/keys :req-un [::transaction-id
                                         ::transaction-date
                                         ::image-id]
                                :opt-un [::caption]))
(s/def ::existing-attachment (s/keys :req-un [::id]
                                     :opt-un [::caption]))

(defn- after-read
  [attachment & _]
  (when attachment
    (storage/tag attachment ::models/attachment)))

(defn- before-save
  [attachment & _]
  (storage/tag attachment ::models/attachment))

(defn create
  [storage attachment]
  (with-storage [s storage]
    (with-validation attachment ::new-attachment []
      (as-> attachment a
        (before-save a)
        (storage/create s a)
        (after-read a)))))

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

(defn update
  [storage-spec attachment]
  (with-storage [s storage-spec]
    (with-validation attachment ::existing-attachment []
      (storage/update s attachment)
      (find-by-id s (:id attachment)))))

(defn delete
  [storage-spec id-or-attachment]
  (with-transacted-storage [s storage-spec]
    (let [attachment (if (integer? id-or-attachment)
                       (find-by-id s id-or-attachment)
                       id-or-attachment)
          image (images/find-by s {:id (:image-id attachment)})]
      (images/delete s image)
      (storage/delete s attachment))))
