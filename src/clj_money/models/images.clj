(ns clj-money.models.images
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [config.core :refer [env]]
            [digest :refer [sha-1]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models]))

(declare find-by-hash)

(defn- body-hash-is-unique?
  [img]
  (= 0 (count (models/select (select-keys img [:image/body-hash :image/user])))))
(v/reg-spec body-hash-is-unique? {:message "The image has already been added"
                                  :path [:image/body-hash]})

(s/def :image/user ::models/model-ref)
(s/def :image/original-filename string?)
(s/def :image/content-type string?)
(s/def :image/body-hash string?)
(s/def :image/body bytes?)
(s/def ::models/image (s/and (s/keys :req [:image/user
                                           :image/original-filename
                                           :image/content-type
                                           :image/body-hash
                                           :image/body])
                             body-hash-is-unique?))

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "search is deprecated"))))

(defn ^:deprecated find-by
  ([criteria] (find-by criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "find-by is deprecated"))))

(defn ^:deprecated find
  [_image-or-id]
  (throw (UnsupportedOperationException. "find is deprecated")))

(defmethod models/before-validation :image
  [{:image/keys [body] :as image}]
  (assoc image :image/body-hash (sha-1 body)))

(defn ^:deprecated create
  [_image]
  (throw (UnsupportedOperationException. "create is deprecated")))

(defn find-or-create
  [image]
  (let [hash (sha-1 (:body image))]
    (with-storage (env :db)
      (or
       (find-by-hash (:user-id image) hash)
       (create image)))))

(defn ^:deprecated delete
  [image]
  (with-storage (env :db)
    (storage/delete image)))
