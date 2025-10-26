(ns clj-money.entities.images
  (:refer-clojure :exclude [update find])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [dgknght.app-lib.validation :as v]
            [clj-money.images :as images]
            [clj-money.entities :as entities]))

(defn- image-is-unique?
  [img]
  (= 0 (count (entities/select (select-keys img [:image/uuid :image/user])))))
(v/reg-spec image-is-unique? {:message "The image has already been added"
                                  :path [:image/uuid]})

(s/def :image/user ::entities/entity-ref)
(s/def :image/original-filename string?)
(s/def :image/content-type string?)
(s/def :image/uuid string?)
(s/def ::entities/image (s/and (s/keys :req [:image/user
                                           :image/original-filename
                                           :image/content-type
                                           :image/uuid])
                             image-is-unique?))

(defn find-or-create
  [{:image/keys [content user] :as image}]
  {:pre [(:image/content image)
         (:image/user image)]}
  (let [uuid (images/put content)]
    (or
      (entities/find-by {:image/uuid uuid
                       :image/user user})
      (-> image
          (assoc :image/uuid uuid)
          (dissoc :image/content)
          entities/put))))
