(ns clj-money.api.imports
  (:refer-clojure :exclude [update get])
  (:require [cljs-http.client :as http]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api-async :as lib-api]
            [clj-money.state :refer [app-state]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- ->multipart-params
  [{:keys [files] :as import-data}]
  (->> files
       (map-indexed (fn [idx f] [idx f]))
       (reduce (fn [m [index file]]
                 (assoc m
                        (keyword (str "source-file-" index))
                        file))
               (dissoc import-data :files))))

(defn create
  [import-data & {:as opts}]
  (let [params (-> import-data
                   ->multipart-params
                   (update-in-if [:options] (comp #(.stringify js/JSON %)
                                                  clj->js)))]
    (http/post (api/path :imports)
               (-> (lib-api/request opts)
                   (lib-api/multipart-params params)
                   (add-error-handler "Unable to create the import: %s")
                   (assoc :oauth-token (:auth-token @app-state))))))

(defn get
  [id & {:as opts}]
  (api/get (api/path :imports id)
           {}
           (add-error-handler
             opts
             "Unable to retrieve the import: %s")))

(defn select
  [criteria & {:as opts}]
  (api/get (api/path :imports)
           criteria
           (add-error-handler
             opts
             "Unable to retrieve the imports: %s")))

(defn delete
  [import & {:as opts}]
  (api/delete (api/path :imports import)
              (add-error-handler
                opts
                "Unable to delete the import: %s")))

(defn start
  [import & {:as opts}]
  (api/patch (api/path :imports import)
             {}
             (add-error-handler
               opts
               "Unable to start the import: %s")))
