(ns clj-money.api
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [ring.util.response :refer [status response header]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize]]))

(defn ->response
  ([value] (->response value 200))
  ([value status-code]
   (-> value
       #_json/generate-string ; TODO decide if this should be handled here or in middleware
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

(defn invalid->response
  [model]
  (->response {:message (validation/error-messages model)} 422))

(defn delete-resource
  [id find-fn delete-fn]
  (let [resource (find-fn (env :db) id) #_(authorize (find-fn (env :db) id) :delete)]
    (try

      (pprint {:delete resource})

      (delete-fn (env :db) (:id resource))
      (catch Exception e

        (pprint {:error e})

        (error->response e "Unable to delete the resource.")))
    {:status 204
     :body []}))

(defn log-error
  [error message]
  (log/error message
             ": "
             (.getClass error)
             " - "
             (.getMessage error)
             "\n  "
             (->> (.getStackTrace error)
                  (map str)
                  (clojure.string/join "\n  "))))
