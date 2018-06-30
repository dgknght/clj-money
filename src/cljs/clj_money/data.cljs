(ns clj-money.data
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clojure.walk :refer [keywordize-keys]]
            [cognitect.transit :as transit]
            [clj-money.util :as util]))

(defn- path
  [& segments]
  (apply util/path (concat [:api] segments)))

(defn- get-models
  [path success-fn]
  (go (let [response (<! (http/get path {:headers {"Content-Type" "application/json"
                                                   "Accept" "application/json"}}))]
        (if (= 200 (:status response))
          (success-fn (:body response))
          (.log js/console "Unable to get the resources from the service" (:body response))))))

(defn create-model
  [path model success-fn error-fn]
  (go (let [response (<! (http/post path {:json-params model
                                          :headers {"Content-Type" "application/json"
                                                    "Accept" "application/json"}}))]
        (if (= 201 (:status response))
          (success-fn (:body response))
          (do
            (.log js/console "Unable to create the model " (prn-str model) ": " (prn-str response))
            (error-fn (-> response :body :message)))))))

(defn get-entities
  [success-fn]
  (get-models (path :entities) success-fn))

(defn create-entity
  [entity success-fn error-fn]
  (create-model (path :entities) entity success-fn error-fn))

(defn update-entity
  ([entity success-fn error-fn]
   (go (let [response (<! (http/patch (path :entities (:id entity)) {:json-params entity
                                                                     :headers {"Content-Type" "application/json"
                                                                               "Accept" "application/json"}}))]
         (if (= 200 (:status response))
           (success-fn (:body response))
           (do
             (.log js/console "Unable to update the entity " (prn-str entity) ": " (prn-str response))
             (error-fn (-> response :body :message))))))))

(defn delete-entity
  [entity success-fn error-fn]
  (go (let [response (<! (http/delete (path :entities
                                            (:id entity))
                                      {:headers {"Content-Type" "application/json"
                                                 "Accept" "application/json"}}))]
        (case (:status response)
          204
          (success-fn)
          404
          (error-fn "The specified resource could not be found on the server.")

          (error-fn (-> response :body :message))))))

(defn get-commodities
  [entity-id success-fn]
  (get-models (path :entities entity-id :commodities) success-fn))

(defn create-commodity
  [commodity success-fn error-fn]

  (.log js/console "create-commodity " (prn-str commodity))

  (create-model (path :entities (:entity-id commodity) :commodities)
                commodity
                success-fn
                error-fn))
