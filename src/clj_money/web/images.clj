(ns clj-money.web.images
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [ring.util.response :as res]
            [clj-money.authorization :as authorization]
            [clj-money.images :as images]
            [clj-money.images.sql]
            [clj-money.images.s3]
            [clj-money.entities :as entities]
            [clj-money.authorization.images]))

(defn- find-image
  "Finds the image the user may see: their own, or one shared with an entity
  they've been granted (see clj-money.authorization.images)"
  [{:keys [params authenticated]}]
  (when-let [image (some-> params
                           (select-keys [:id])
                           (entities/find-by :entity-type :image))]
    (when (authorization/allowed? image ::authorization/show authenticated)
      image)))

(defn- text-not-found
  [msg]
  (-> msg
      res/response
      (res/status 404)
      (res/content-type "text/plain")))

(def ^:private not-found
  (text-not-found "not found"))

; The image record exists, but its content can't be found in image
; storage (e.g., the storage was changed without copying its contents)
(def ^:private content-not-found
  (text-not-found "image content not found"))

(defn- ->response
  [{:image/keys [uuid content-type] :keys [id]}]
  (if-let [content (images/get uuid)]
    (-> content
        io/input-stream
        res/response
        (res/content-type content-type))
    (do
      (log/warnf "[images] content not found for image %s with uuid %s" id uuid)
      content-not-found)))

(defn show
  [req]
  (if-let [image (find-image req)]
    (->response image)
    not-found))

(def routes
  ["images/:id" {:get {:handler show}}])
