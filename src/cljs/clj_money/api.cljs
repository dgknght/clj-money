(ns clj-money.api
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clojure.walk :refer [keywordize-keys]]
            [cognitect.transit :as transit]
            [clj-money.util :as util]))

(defn path
  [& segments]
  (apply util/path (concat [:api] segments)))

(defn get-resources
  [path success-fn]
  (go (let [response (<! (http/get path {:headers {"Content-Type" "application/json"
                                                   "Accept" "application/json"}}))]
        (if (= 200 (:status response))
          (success-fn (:body response))
          (.log js/console "Unable to get the resources from the service" (:body response))))))

(defn create-resource
  [path model success-fn error-fn]
  (go (let [response (<! (http/post path {:json-params model
                                          :headers {"Content-Type" "application/json"
                                                    "Accept" "application/json"}}))]
        (if (= 201 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to create the model " (prn-str model) ": " (prn-str response))
            (error-fn (-> response :body :message)))))))

(defn update-resource
  [path model success-fn error-fn]
  (go (let [response (<! (http/patch path {:json-params model
                                           :headers {"Content-Type" "application/json"
                                                     "Accept" "application/json"}}))]
        (if (= 200 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to update the model " (prn-str model) ": " (prn-str response))
            (error-fn (-> response :body :message)))))))

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
