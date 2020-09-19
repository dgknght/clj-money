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

(defn- infer-status-code
  [data]
  (if (and (map? data)
           (validation/has-error? data))
    400
    200))

(defn ->response
  ([]
   (-> (response {})
       (status 204)
       (header "Content-Type" "application/json")))
  ([value] (->response value (infer-status-code value)))
  ([value status-code]
   (-> value
       response
       (header "Content-Type" "application/json")
       (status status-code))))

(defn creation-response
  [value]
  (->response value (if (and (map? value)
                             (validation/has-error? value))
                      400
                      201)))

(defn unauthorized []
  (->response {:message "unauthorized"} 401))

(defn not-found []
  (->response {:message "not found"} 404))

(defn bad-request
  ([] (bad-request "bad request"))
  ([message]
   (->response {:message message} 400)))

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
  (let [resource (authorize (find-fn id)
                            ::authorization/destroy
                            user)]
    (try
      (delete-fn (:id resource))
      (catch Exception e
        (error->response e "Unable to delete the resource.")))
    (->response)))

(defn log-error
  [error message]
  (log/errorf "%s: %s - %s\n  %s"
              message
              (.getClass error)
              (.getMessage error)
              (->> (.getStackTrace error)
                   (map str)
                   (clojure.string/join "\n  "))))

(defn- extract-header-auth-token
  [{:keys [headers]}]
  (when-let [header-value (get-in headers ["authorization"])]
    (re-find #"(?<=Bearer ).*" header-value)))

(defn- extract-cookie-auth-token
  [{:keys [cookies]}]
  (get-in cookies ["auth-token" :value]))

(defn- extract-auth-token
  [req]
  (some #(% req) [extract-header-auth-token
                  extract-cookie-auth-token]))

(defn- find-user-by-auth-token
  [req]
  (when-let [token (extract-auth-token req)]
    (users/find (:user-id (jwt/unsign token
                                      (env :secret))))))

(defn wrap-authentication
  [handler]
  (fn [req]
    (if-let [user (find-user-by-auth-token req)]
      (handler (assoc req :authenticated user))
      (unauthorized))))
