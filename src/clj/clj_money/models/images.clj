(ns clj-money.models.images
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [environ.core :refer [env]]
            [digest :refer [sha-1]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.util :refer [->id]]
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
  (tag image ::models/image))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/image)
                          options)))))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn find
  [image-or-id]
  (find-by {:id (->id image-or-id)} {:include-body? true}))

(defn- find-by-hash
  [user-id hash]
  (find-by {:user-id user-id
            :body-hash hash}))

(defn- body-hash-is-unique?
  [{:keys [body-hash user-id]}]
  (nil? (find-by-hash user-id body-hash)))

(def ^:private validation-rules
  [(validation/create-rule body-hash-is-unique?
                           [:body-hash]
                           "The image content must be unique")])

(defn- before-validation
  [image]
  (assoc image :body-hash (sha-1 (:body image))))

(defn- before-save
  [image]
  (tag image ::models/image))

(defn create
  [image]
  (with-storage (env :db)
    (let [image (before-validation image)]
      (with-validation image ::image validation-rules
        (-> image
            before-save
            storage/create
            after-read)))))

(defn find-or-create
  [image]
  (let [hash (sha-1 (:body image))]
    (with-storage (env :db)
      (or
       (find-by-hash (:user-id image) hash)
       (create image)))))

(defn delete
  [image]
  (with-storage (env :db)
    (storage/delete image)))
