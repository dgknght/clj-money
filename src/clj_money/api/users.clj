(ns clj-money.api.users
  (:refer-clojure :exclude [find])
  (:require [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.web.auth :refer [make-token]]
            [clj-money.entities.users :as users]))

(defn- index
  [{:keys [authenticated]}]
  (if (contains? (:user/roles authenticated) :admin)
    (api/response (entities/select (util/entity-type {} :user)))
    api/forbidden))

(defn- destroy
  [{:keys [authenticated params]}]
  (if (contains? (:user/roles authenticated) :admin)
    (if-let [user (entities/find-by
                    (util/entity-type (select-keys params [:id]) :user))]
      (do
        (entities/delete user)
        (api/response))
      api/not-found)
    api/forbidden))

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
  [["users"
    ["" {:get {:handler index}}]
    ["/me" {:get {:handler find}}]
    ["/:id" {:delete {:handler destroy}}]]])

(def unauthenticated-routes
  ["users/authenticate" {:post {:handler authenticate}}])
