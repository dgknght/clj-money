(ns clj-money.web.images
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [ring.util.response :as res]
            [clj-money.authorization :refer [authorize +scope] :as authorization]
            [clj-money.models :as models]
            [clj-money.authorization.images]))

(defn- find-image
  [{:keys [params authenticated]}]
  (some-> params
          (select-keys [:id])
          (+scope :image authenticated)
          (models/find-by {:include-body? true})))

(defn- ->response
  [{:image/keys [body content-type]}]
  (-> (io/input-stream body)
      res/response
      (res/content-type content-type)))

(def ^:private not-found
  (-> "not found"
      res/response
      (res/status 404)
      (res/content-type "text/plain")))

(defn show
  [{:as req :keys [authenticated]}]
  (or (some-> req
              find-image
              (authorize ::authorization/show authenticated)
              ->response)
      not-found))

(def routes
  ["images/:id" {:get {:handler show}}])
