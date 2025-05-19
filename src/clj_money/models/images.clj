(ns clj-money.models.images
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
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
(s/def :image/uuid string?)
(s/def ::models/image (s/and (s/keys :req [:image/user
                                           :image/original-filename
                                           :image/content-type
                                           :image/uuid])
                             body-hash-is-unique?))

(defn find-or-create
  [_image]
  (throw (UnsupportedOperationException. "Not implemented yet"))
  #_(let [hash (sha-1 body)]
    (or
      (models/find-by {:image/body-hash hash
                       :image/user user})
      (models/put image))))
