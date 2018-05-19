(ns clj-money.data
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [clojure.string :as string]
            [clojure.walk :refer [keywordize-keys]]
            [cognitect.transit :as transit]))

(defn- stringify
  [value]
  (if (keyword? value)
    (name value)
    (.toString value)))

(defn- path
  [& segments]
  (str "/" (->> (concat [:api] segments)
                (map stringify)
                (string/join "/"))))

(defn get-entities
  [callback]
  (go (let [response (<! (http/get (path :entities) {:headers {"Content-Type" "application/json"
                                                               "Accept" "application/json"}}))]
        (if (= 200 (:status response))
          (callback (:body response))
          (.log js/console "Unable to get the entities from the service" (:body response))))))

(defn update-entity
  ([entity success-fn error-fn]
   (go (let [response (<! (http/patch (path :entities (:id entity) {:headers {"Content-Type" "application/json"
                                                                              "Accept" "application/json"}})))]
         (if (= 204 (:status response))
           (success-fn)
           (do
             (.log js/console "Unable to update the entity " (prn-str entity) ": " (:body response))
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
