(ns clj-money.models.images
  (:refer-clojure :exclude [update find])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [dgknght.app-lib.validation :as v]
            [clj-money.images :as images]
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
