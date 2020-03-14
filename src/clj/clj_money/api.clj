(ns clj-money.api
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [ring.util.response :refer [status response header]]
            [environ.core :refer [env]]
            [buddy.sign.jwt :as jwt]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize] :as authorization]
            [clj-money.models.users :as users]))

(defn ->response
  ([]
   (-> (response {})
       (status 204)
       (header "Content-Type" "application/json")))
  ([value] (->response value 200))
  ([value status-code]
   (-> value
       response
       (header "Content-Type" "application/json")
       (status status-code))))

(defn unauthorized []
  (->response {:message "unauthorized"} 401))

(defn not-found []
  (->response {:message "not found"} 404))

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
  [id user find-fn delete-fn]
  (let [resource (authorize (find-fn (env :db) id)
                            ::authorization/destroy
                            user)]
    (try
      (delete-fn (env :db) (:id resource))
      (catch Exception e
        (error->response e "Unable to delete the resource.")))
    (->response)))

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

(defn- find-user-by-auth-token
  [{:keys [headers]}]
  (when-let [header-value (get-in headers ["authorization"])]
    (when-let [token (re-find #"(?<=Bearer ).*" header-value)]
      (users/find-by-id (env :db)
                        (:user-id (jwt/unsign token
                                              (env :secret)))))))

(defn wrap-authentication
  [handler]
  (fn [req]
    (if-let [user (find-user-by-auth-token req)]
      (handler (assoc req :authenticated user))
      (unauthorized))))
