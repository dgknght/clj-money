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

(defn- create-admin
  [{:keys [params]}]
  (if (pos? (entities/count (util/entity-type {} :user)))
    api/forbidden
    (let [user (-> params
                   (select-keys [:user/first-name
                                 :user/last-name
                                 :user/email
                                 :user/password])
                   (assoc :user/roles #{:admin})
                   entities/put)]
      (api/creation-response (->auth-response user)))))

(def routes
  [["users"
    ["" {:get {:handler index}}]
    ["/me" {:get {:handler find}}]]])

(def unauthenticated-routes
  ["users"
   ["/authenticate" {:post {:handler authenticate}}]
   ["/admin" {:post {:handler create-admin}}]])
