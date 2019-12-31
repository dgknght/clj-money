(ns clj-money.api
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clj-money.x-platform.util :refer [map->query-string]]
            [clj-money.util :as util]))

(defn path
  [& segments]
  (apply util/path (concat [:api] segments)))

(defn- append-query-string
  [path criteria]
  (if (empty? criteria)
    path
    (let [query-string (map->query-string criteria)]
      (str path "?" query-string))))

(defn get-resources
  ([path success-fn error-fn]
   (get-resources path {} success-fn error-fn))
  ([path criteria success-fn error-fn]
   (get-resources path criteria {} success-fn error-fn))
  ([path criteria options success-fn error-fn]
   (go (let [params (cond-> {:criteria criteria}
                      (seq options)
                      (assoc :options options))
             response (<! (http/get (append-query-string path params)
                                    {:headers {"Content-Type" "application/json"
                                               "Accept" "application/json"}}))]
         (if (= 200 (:status response))
           (success-fn (:body response))
           (do
             (.log js/console
                   "Unable to get the resource(s) at "
                   path
                   " from the service: "
                   (:body response))
             (error-fn (-> response :body :message))))))))

(defn- extract-error
  [response]
  (some #(% response)
        [(comp :message :body)
         :error-text]))

(defn create-resource
  [path model success-fn error-fn]
  (go (let [response (<! (http/post path {:json-params model
                                          :headers {"Content-Type" "application/json"
                                                    "Accept" "application/json"}}))]
        (if (= 201 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to create the model " (prn-str model) ": " (prn-str response))
            (.log js/console "Unable to create the model")
            (.log js/console (prn-str model))
            (.log js/console (prn-str response))
            (error-fn (extract-error response)))))))

(defn update-resource
  [path model success-fn error-fn]
  (go (let [response (<! (http/patch path {:json-params model
                                           :headers {"Content-Type" "application/json"
                                                     "Accept" "application/json"}}))]
        (if (= 200 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to update the model " (prn-str model) ": " (prn-str response))
            (error-fn (extract-error response)))))))

(defn delete-resource
  [path success-fn error-fn]
  (go (let [response (<! (http/delete path
                                      {:headers {"Content-Type" "application/json"
                                                 "Accept" "application/json"}}))]
        (case (:status response)
          204
          (success-fn)
          404
          (error-fn "The specified resource could not be found on the server.")

          (error-fn (-> response :body :message))))))
