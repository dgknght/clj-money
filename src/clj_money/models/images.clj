(ns clj-money.models.images
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [digest :refer [sha-1]]
            [clj-money.util :refer [rev-args]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :as authorization]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-image
                                              select-images
                                              find-image-by-id
                                              delete-image]]))

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

(defn- find-by-hash
  [storage user-id hash]
  (first (select-images storage
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

(def create
  (create-fn {:create (rev-args create-image)
              :spec ::image
              :before-validation before-validation
              :rules-fn validation-rules}))

(defn find-or-create
  [storage-spec image]
  (let [hash (sha-1 (:body image))]
    (with-storage [s storage-spec]
      (or
        (find-by-hash s (:user-id image) hash)
        (create s image)))))

(defn find-by-id
  [storage-spec id]
  (with-storage [s storage-spec]
    (authorization/tag-resource (find-image-by-id s id) :image)))

(defn delete
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-image s id)))
