(ns clj-money.api.accounts
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as storage]
            [clj-money.api :refer [->response
                                   not-found]]
            [clj-money.models :as models]
            [clj-money.authorization :refer [authorize
                                             +scope]
             :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.authorization.accounts]))

(defn- index
  [{:keys [params authenticated]}]
  (->response (accounts/search (env :db) (-> params
                                             (select-keys [:entity-id])
                                             (+scope ::models/account authenticated)))))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (authorize (accounts/find-by-id (env :db)
                                  (:id params))
             action
             authenticated))

(defn- show
  [req]
  (->response (find-and-auth req ::authorization/show)))

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
      (update-in [:type] keyword)
      (select-keys attribute-keys)
      (storage/tag ::models/account)))

(defn- create
  [{:keys [params body authenticated]}]
  (let [account (-> body
                    (assoc :entity-id (:entity-id params))
                    before-save
                    (authorize ::authorization/create authenticated))]
    (->response (accounts/create (env :db) account)
                201)))

(defn- update
  [{:keys [body] :as req}]
  (if-let [account (find-and-auth req ::authorization/update)]
    (->response (accounts/update
                  (env :db)
                  (merge account (-> body
                                     (select-keys attribute-keys)
                                     before-save))))
    (not-found)))

(defn- delete
  [req]
  (if-let [account (find-and-auth req ::authorization/destroy)]
    (do
      (accounts/delete (env :db) account)
      (->response))
    (not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/accounts" req (index req))
  (POST "/api/entities/:entity-id/accounts" req (create req))
  (GET "/api/accounts/:id" req (show req))
  (PATCH "/api/accounts/:id" req (update req))
  (DELETE "/api/accounts/:id" req (delete req)))
