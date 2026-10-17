(ns clj-money.entities.images
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [dgknght.app-lib.validation :as v]
            [clj-money.io :refer [read-bytes]]
            [clj-money.images :as images]
            [clj-money.entities :as entities]))

(defn- image-is-unique?
  [img]
  (zero? (entities/count (select-keys img [:image/uuid :image/user]))))
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

(defn find-or-create-upload
  "Given a file uploaded in a multipart request, stores its content and
  returns the image for it"
  [{:keys [filename content-type tempfile] :as upload} user]
  (when upload
    (find-or-create #:image{:user user
                            :original-filename filename
                            :content-type content-type
                            :content (read-bytes tempfile)})))
