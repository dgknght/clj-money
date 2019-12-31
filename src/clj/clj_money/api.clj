(ns clj-money.api
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [ring.util.response :refer [status response header]]
            [environ.core :refer [env]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize
                                             tag-resource
                                             apply-scope]]))

(defn ->response
  ([value] (->response value 200))
  ([value status-code]
   (-> value
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

(defn index-resource
  [search-fn params resource-type]
  (->response (search-fn
                (env :db)
                (apply-scope params resource-type))))

(defn- extract-validation-messages
  [model]
  {:message (str "The resource is not valid. " (validation/error-messages model))})

(defn create-resource
  [resource-type params create-fn]
  (let [resource (-> params
                     (tag-resource resource-type)
                     (authorize :create))
        result (create-fn (env :db) resource)]
    (if (validation/has-error? result)
      (status (->response (extract-validation-messages result)) 422)
      (status (->response result) 201))))

(defn update-resource
  [params find-fn update-fn]
  (if-let [resource (find-fn (env :db) params)]
    (let [to-update (-> resource
                        (authorize :update)
                        (merge params))
          result (update-fn (env :db) to-update)]
      (if (validation/has-error? result)
        (status (->response result) 422)
        (status (->response result) 200)))
    (status (->response {}) 404)))

(defn delete-resource
  [id find-fn delete-fn]
  (let [resource (authorize (find-fn (env :db) id) :delete)]
    (try
      (delete-fn (env :db) (:id resource))
      (catch Exception e
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
