(ns clj-money.web.images
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [ring.util.response :as res]
            [clj-money.authorization :as authorization]
            [clj-money.util :as util]
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
                           (util/entity-type :image)
                           entities/find-by)]
    (when (authorization/allowed? image ::authorization/show authenticated)
      image)))

(defn- ->response
  [{:image/keys [uuid content-type]}]
  (-> (images/get uuid)
      io/input-stream
      res/response
      (res/content-type content-type)))

(def ^:private not-found
  (-> "not found"
      res/response
      (res/status 404)
      (res/content-type "text/plain")))

(defn show
  [req]
  (or (some-> req
              find-image
              ->response)
      not-found))

(def routes
  ["images/:id" {:get {:handler show}}])
