(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clj-money.api :as api]
            [clj-money.util :as util]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]))

(defn- ->multipart-params
  [import-data]
  (reduce (fn [m [index file]]
            (assoc m
                   (keyword (str "source-file-" index))
                   file))
          (dissoc import-data :files)
          (map-indexed (fn [idx f] [idx f]) (:files import-data))))

(defn- after-read
  [imp]
  (-> imp
      (update-in [:created-at] util/parse-date-time )
      (update-in [:updated-at] util/parse-date-time)))

(defn create
  [import-data success-fn error-fn]
  (let [params (->multipart-params import-data)]
    (go (let [response (<! (http/post "/api/imports"
                                      (-> {}
                                          (api/multipart-params params)
                                          api/append-auth)))]
          (if (= 201 (:status response))
            (success-fn (update-in (:body response) [:import] after-read))
            (do
              (.log js/console "Unable to create the import " (prn-str response))
              (error-fn (-> response :body :message))))))))

(defn get-one
  [id success-fn error-fn]
  (api/get-resources (api/path :imports id)
                     #(success-fn (after-read %))
                     error-fn))

(defn get-all
  [success-fn error-fn]
  (api/get-resources (api/path :imports)
                     #(success-fn (map after-read %))
                     error-fn))

(defn delete
  [{id :id} success-fn error-fn]
  (api/delete-resource (api/path :imports id)
                       success-fn
                       error-fn))

(defn start
  [{id :id} success-fn error-fn]
  (go (let [path (str "/api/imports/" id "/start")
            response (<! (http/put path {:headers {"Content-Type" "application/json"
                                                   "Accept" "application/json"}}))]
        (if (= 200 (:status response))
          (success-fn)
          (do
            (.log js/console "Unable to start import " id ": " (prn-str response))
            (error-fn (-> response :body :message)))))))
