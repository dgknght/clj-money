(ns clj-money.api.imports
  (:refer-clojure :exclude [update get])
  (:require #_[cljs-http.client :as http]
            #_[clj-money.state :refer [app-state]]
            [clj-money.api :as api :refer [handle-ex]]))

#_(defn- ->multipart-params
  [{:keys [files] :as import-data}]
  (->> files
       (map-indexed (fn [idx f] [idx f]))
       (reduce (fn [m [index file]]
                 (assoc m
                        (keyword (str "source-file-" index))
                        file))
               (dissoc import-data :files))))

(defn create
  [_import-data & {:as _opts}]
  (throw (js/Error. "Not implemented"))
  #_(let [params (-> import-data
                   ->multipart-params
                   (update-in-if [:options] (comp #(.stringify js/JSON %)
                                                  clj->js)))]
    (http/post (api/path :imports)
               (-> (lib-api/request {:transform (comp (api/apply-fn after-create)
                                                      xf)
                                     :handle-ex (handle-ex "Unable to create the import: %s")})
                   (lib-api/multipart-params params)
                   (assoc :oauth-token (:auth-token @app-state))))))

(defn get
  [id & {:as opts}]
  (api/get (api/path :imports id)
           {}
           (merge
             {:on-error (handle-ex "Unable to retrieve the import: %s")}
             opts)))

(defn select
  [& {:as opts}]
  (api/get (api/path :imports)
           {}
           (merge
             {:on-error (handle-ex "Unable to retrieve the imports: %s")}
             opts)))

(defn delete
  [{:keys [id]} & {:as opts}]
  (api/delete (api/path :imports id)
              (merge
                {:on-error (handle-ex "Unable to delete the import: %s")}
                opts)))

(defn start
  [{:keys [_id]} & {:as _opts}]
  (throw (js/Error. "Not implemented"))
  #_(let [path (api/path :imports id)]
    (http/patch path
                (assoc (lib-api/request
                         {:transform (transform xf)
                          :handle-ex (handle-ex "Unable to start the import: %s")})
                       :oauth-token (:auth-token @app-state)))))
