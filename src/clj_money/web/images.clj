(ns clj-money.web.images
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-money.models.images :as images]
            [clj-money.authorization :refer [authorize]]))

(defn show
  [{{image-id :image-id} :params}]
  (let [image (authorize (images/find-by-id (env :db) image-id) :show)]
    (if image
      (-> (io/input-stream (:body image))
          response
          (content-type (:content-type image)))
      (-> "not found"
          response
          (content-type "text/plain")))))
