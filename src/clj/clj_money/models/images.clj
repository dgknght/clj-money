(ns clj-money.models.images
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [digest :refer [sha-1]]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.models :as models]))

(s/def ::user-id integer?)
(s/def ::original-filename validation/non-empty-string?)
(s/def ::content-type string?)
(s/def ::body-hash validation/non-empty-string?)
(s/def ::body bytes?)
(s/def ::image (s/keys :req-un [::user-id
                                ::original-filename
                                ::content-type
                                ::body-hash
                                ::body]))

(defn- after-read
  [image]
  (storage/tag image ::models/image))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read
          (storage/select s
                          (storage/tag criteria ::models/image)
                          options)))))

(defn- find-by-hash
  [storage user-id hash]
  (first (search storage
                 {:user-id user-id
                  :body-hash hash}
                 {:limit 1})))

(defn- body-hash-is-unique?
  [storage {:keys [body-hash user-id]}]
  (nil? (find-by-hash storage user-id body-hash)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial body-hash-is-unique? storage)
                           [:body-hash]
                           "The image content must be unique")])

(defn- before-validation
  [image & _]
  (assoc image :body-hash (sha-1 (:body image))))

(defn- before-save
  [image & _]
  (storage/tag image ::models/image))

(defn create
  [storage image]
  (with-storage [s storage]
    (let [image (before-validation image)]
      (with-validation image ::image (validation-rules s)
        (as-> image i
          (before-save i)
          (storage/create s i)
          (after-read i))))))

(defn find-or-create
  [storage-spec image]
  (let [hash (sha-1 (:body image))]
    (with-storage [s storage-spec]
      (or
        (find-by-hash s (:user-id image) hash)
        (create s image)))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (merge options {:limit 1})))))

(defn find-by-id
  [storage-spec id]
  (find-by storage-spec {:id id} {:include-body? true}))

(defn delete
  [storage-spec image]
  (with-storage [s storage-spec]
    (storage/delete s image)))
