(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [clj-money.api :refer [->response
                                   not-found]]
            [clj-money.authorization :refer [authorize
                                             apply-scope
                                             tag-resource]]
            [clj-money.models.accounts :as accounts]
            [clj-money.permissions.accounts]))

(defn- index
  [{:keys [params authenticated]}]
  (->response (accounts/search (env :db) (-> params
                                             (select-keys [:entity-id])
                                             (apply-scope :account authenticated)))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (authorize (accounts/find-by-id (env :db)
                                  (:id params))
             action
             authenticated))

(defn- show
  [req]
  (->response (find-and-auth req :show)))

(def ^:private attribute-keys
  [:id
   :name
   :entity-id
   :type
   :commodity-id
   :tags
   :parent-id])

(defn- before-save
  [account]
  (-> account
      (update-in [:tags] #(if (:trading account)
                            (conj (or % #{}) :trading)
                            %))
      (select-keys attribute-keys)
      (tag-resource :account)))

(defn- create
  [{:keys [params body authenticated]}]
  (let [account (-> body
                    (assoc :entity-id (:entity-id params))
                    before-save
                    (authorize :create authenticated))]
    (->response (accounts/create (env :db) account)
                201)))

(defn- update
  [{:keys [body] :as req}]
  (if-let [account (find-and-auth req :update)]
    (->response (accounts/update (env :db)
                                 (merge account
                                        (select-keys body attribute-keys))))
    (not-found)))

(defn- delete
  [req]
  (if-let [account (find-and-auth req :delete)]
    (do
      (accounts/delete (env :db) (:id account))
      (->response))
    (not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/accounts" req (index req))
  (POST "/api/entities/:entity-id/accounts" req (create req))
  (GET "/api/accounts/:id" req (show req))
  (PATCH "/api/accounts/:id" req (update req))
  (DELETE "/api/accounts/:id" req (delete req)))
