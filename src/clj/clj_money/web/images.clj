(ns clj-money.web.images
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [environ.core :refer [env]]
            [ring.util.response :refer [response content-type]]
            [clj-money.models.images :as images]
            [clj-money.authorization.images]
            [clj-money.authorization :refer [authorize] :as authorization]))

(defn show
  [{{image-id :image-id} :params :keys [authenticated]}]
  (let [image (authorize (images/find-by-id (env :db) image-id) authenticated ::authorization/show)]
    (if image
      (-> (io/input-stream (:body image))
          response
          (content-type (:content-type image)))
      (-> "not found"
          response
          (content-type "text/plain")))))
