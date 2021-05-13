(ns clj-money.api
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]
            [environ.core :refer [env]]
            [buddy.sign.jwt :as jwt]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.authorization :refer [authorize] :as authorization]
            [clj-money.models.users :as users]))

(defn error->response
  [error safe-error-message]
  (api/response
   (if (env :show-error-messages?)
     {:message (.getMessage error)
      :type (.getName (.getClass error))
      :stack (.getStackTrace error)}
     {:message safe-error-message})
   500))

(defn delete-resource
  [id user find-fn delete-fn]
  (let [resource (authorize (find-fn id)
                            ::authorization/destroy
                            user)]
    (try
      (delete-fn (:id resource))
      (catch Exception e
        (error->response e "Unable to delete the resource.")))
    (api/response)))

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

(defn find-user-by-auth-token
  [req]
  (when-let [token (extract-auth-token req)]
    (users/find (:user-id (jwt/unsign token
                                      (env :secret))))))
