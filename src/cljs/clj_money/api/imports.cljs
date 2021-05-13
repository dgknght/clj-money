(ns clj-money.api.imports
  (:refer-clojure :exclude [update get])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [unserialize-date-time]]
            [dgknght.app-lib.api :as api]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clj-money.state :refer [app-state]]))

(defn- ->multipart-params
  [import-data]
  (->> (:files import-data)
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

(defn create
  [import-data success-fn error-fn]
  (let [params (-> import-data
                   ->multipart-params
                   (update-in-if [:options] (comp #(.stringify js/JSON %)
                                               clj->js)))]
    (go (let [response (<! (http/post (api/path :imports)
                                      (-> {}
                                          (api/multipart-params params)
                                          (assoc :oauth-token (:auth-token @app-state)))))]
          (if (= 201 (:status response))
            (success-fn (update-in (:body response) [:import] after-read))
            (do
              (.log js/console "Unable to create the import " (prn-str response))
              (error-fn (-> response :body :message))))))))

(defn get
  [id success-fn error-fn]
  (api/get (api/path :imports id)
           (comp success-fn after-read)
           error-fn))

(defn select
  [success-fn error-fn]
  (api/get (api/path :imports)
           (comp success-fn
                 #(map after-read %))
           error-fn))

(defn delete
  [{id :id} success-fn error-fn]
  (api/delete (api/path :imports id)
              success-fn
              error-fn))

(defn start
  [{id :id} success-fn error-fn]
  (go (let [path (api/path :imports id)
            response (<! (http/patch path
                                     (assoc (api/request) :oauth-token (:auth-token @app-state))))]
        (if (= 200 (:status response))
          (success-fn)
          (do
            (.log js/console "Unable to start import " id ": " (prn-str response))
            (error-fn (-> response :body :message)))))))
