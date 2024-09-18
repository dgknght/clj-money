(ns clj-money.api.imports
  (:refer-clojure :exclude [update get])
  (:require [cljs-http.client :as http]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [unserialize-date-time]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.state :refer [app-state]]
            [clj-money.api :refer [handle-ex]]))

(defn- ->multipart-params
  [{:keys [files] :as import-data}]
  (->> files
       (map-indexed (fn [idx f] [idx f]))
       (reduce (fn [m [index file]]
                 (assoc m
                        (keyword (str "source-file-" index))
                        file))
               (dissoc import-data :files))))

(defn- after-read
  [imp]
  (-> imp
      (update-in [:created-at] unserialize-date-time)
      (update-in [:updated-at] unserialize-date-time)))

(defn- after-create
  [result]
  (update-in result [:import] after-read))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn create
  [import-data xf]
  (let [params (-> import-data
                   ->multipart-params
                   (update-in-if [:options] (comp #(.stringify js/JSON %)
                                                  clj->js)))]
    (http/post (api/path :imports)
               (-> (api/request {:transform (comp (api/apply-fn after-create)
                                                  xf)
                                 :handle-ex (handle-ex "Unable to create the import: %s")})
                   (api/multipart-params params)
                   (assoc :oauth-token (:auth-token @app-state))))))

(defn get
  [id xf]
  (api/get (api/path :imports id)
           {}
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the import: %s")}))

(defn select
  [xf]
  (api/get (api/path :imports)
           {}
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the imports: %s")}))

(defn delete
  [{id :id} xf]
  (api/delete (api/path :imports id)
              {:transform xf
               :handle-ex (handle-ex "Unable to delete the import: %s")}))

(defn start
  [{id :id} xf]
  (let [path (api/path :imports id)]
    (http/patch path
                (assoc (api/request
                         {:transform (transform xf)
                          :handle-ex (handle-ex "Unable to start the import: %s")})
                       :oauth-token (:auth-token @app-state)))))
