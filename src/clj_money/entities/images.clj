(ns clj-money.entities.images
  (:refer-clojure :exclude [update find])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [dgknght.app-lib.validation :as v]
            [clj-money.images :as images]
            [clj-money.entities :as models]))

(defn- image-is-unique?
  [img]
  (= 0 (count (models/select (select-keys img [:image/uuid :image/user])))))
(v/reg-spec image-is-unique? {:message "The image has already been added"
                                  :path [:image/uuid]})

(s/def :image/user ::models/model-ref)
(s/def :image/original-filename string?)
(s/def :image/content-type string?)
(s/def :image/uuid string?)
(s/def ::models/image (s/and (s/keys :req [:image/user
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
      (models/find-by {:image/uuid uuid
                       :image/user user})
      (-> image
          (assoc :image/uuid uuid)
          (dissoc :image/content)
          models/put))))
