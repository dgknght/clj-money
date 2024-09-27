(ns clj-money.web.images
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [ring.util.response :refer [response content-type]]
            [clj-money.models.images :as images]
            [clj-money.authorization.images]
            [dgknght.app-lib.authorization :refer [authorize] :as authorization]))

(defn show
  [{:keys [params authenticated]}]
  (if-let [image (images/find (:id params))]
    (do
      (authorize image ::authorization/show authenticated)
      (-> (io/input-stream (:body image))
          response
          (content-type (:content-type image))))
    (-> "not found"
        response
        (content-type "text/plain"))))

(def routes
  ["images/:id" {:get {:handler show}}])
