(ns clj-money.api.users
  (:refer-clojure :exclude [find])
  (:require [clojure.set :refer [rename-keys]]
            [compojure.core :refer [defroutes GET POST]]
            [dgknght.app-lib.api :as api]
            [clj-money.web.auth :refer [make-token]]
            [clj-money.models.users :as users]))

(defn- find
  [{:keys [authenticated]}]
  (api/response authenticated))

(defn- extract-credentials
  [{:keys [body]}]
  (-> body
      (select-keys [:email :password])
      (rename-keys {:email :username})))

(defn- authenticate
  [req]
  (if-let [user (users/authenticate (extract-credentials req))]
    (api/creation-response {:user user
                            :auth-token (make-token user)})
    api/not-found))

(defroutes routes
  (GET "/api/users/me" req (find req)))

(defroutes unauthenticated-routes
  (POST "/api/users/authenticate" req (authenticate req)))
