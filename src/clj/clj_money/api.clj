(ns clj-money.api
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [status response header]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-money.authorization :refer [authorize]]))

(defn ->response
  ([value] (->response value 200))
  ([value status-code]
   (-> value
       json/generate-string
       response
       (header "Content-Type" "application/json")
       (status status-code))))

(defn error->response
  [error safe-error-message]
  (->response
    (if (env :show-error-messages?)
      {:message (.getMessage error)
       :type (.getName (.getClass error))
       :stack (.getStackTrace error)}
      {:message safe-error-message})
    500))

(defn delete-resource
  [id find-fn delete-fn]
  (let [model (authorize (find-fn (env :db) id) :delete)]
    (try
      (delete-fn (env :db) (:id model))
      (catch Exception e
        (-> (if (env :show-error-messages?)
              (.getMessage e)
              "Unable to delete the resource")
            response
            (header "Content-Type" "application/json")
            (status 500))))
    {:status 204
     :body []}))
