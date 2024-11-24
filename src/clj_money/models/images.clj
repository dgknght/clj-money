(ns clj-money.models.images
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [digest :refer [sha-1]]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models]))

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

(defmethod models/before-validation :image
  [{:image/keys [body] :as image}]
  (assoc image :image/body-hash (sha-1 body)))

(defn find-or-create
  [{:as image :image/keys [body user]}]
  (let [hash (sha-1 body)]
    (or
      (models/find-by {:image/body-hash hash
                       :image/user user})
      (models/put image))))
