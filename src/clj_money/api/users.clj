(ns clj-money.api.users
  (:refer-clojure :exclude [find])
  (:require [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.api :as api]
            [clj-money.web.auth :refer [make-token]]
            [clj-money.entities.users :as users]))

(defn- find
  [{:keys [authenticated]}]
  (api/response authenticated))

(defn- extract-credentials
  [{:keys [params]}]
  (-> params
      (select-keys [:email :password])
      (rename-keys {:email :username})))

(defn- ->auth-response
  [user]
  {:user user
   :auth-token (make-token user)})

(defn- authenticate
  [req]
  (or (some-> req
              extract-credentials
              users/authenticate
              ->auth-response
              api/creation-response)
      api/not-found))

(def routes
  ["users/me" {:get {:handler find}}])

(def unauthenticated-routes
  ["users/authenticate" {:post {:handler authenticate}}])
