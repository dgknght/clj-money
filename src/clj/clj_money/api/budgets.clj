(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as stow]
            [clj-money.x-platform.util :refer [update-in-if
                                               unserialize-date]]
            [clj-money.api :refer [->response
                                   not-found]]
            [clj-money.models :as models]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.models.budgets :as budgets]
            [clj-money.authorization.budgets]))

(defn- index
  [{:keys [params authenticated]}]
  (->response (budgets/search (env :db)
                              (-> params
                                  (select-keys [:entity-id])
                                  (+scope ::models/budget authenticated))
                              {:sort [[:start-date :desc]]})))
(defn- extract-budget
  [{:keys [body]}]
  (-> body
      (select-keys [:name
                    :start-date
                    :period
                    :period-count
                    :items])
      (update-in-if [:period] keyword)
      (update-in-if [:start-date] unserialize-date)))

(defn- create
  [{:keys [authenticated params] :as req}]
  (as-> req b
    (extract-budget b)
    (assoc b :entity-id (:entity-id params))
    (stow/tag b ::models/budget)
    (authorize b ::auth/create authenticated)
    (budgets/create (env :db) b)
    (->response b 201)))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [budget (budgets/find-by (env :db)
                                     (+scope {:id (:id params)}
                                             ::models/budget
                                             authenticated))]
    (authorize budget action authenticated)))

(defn- find
  [req]
  (if-let [budget (find-and-auth req ::auth/show)]
    (->response budget)
    (not-found)))

(defn- update
  [req]
  (if-let [budget (find-and-auth req ::auth/update)]
    (->response
      (as-> budget b
        (merge b (extract-budget req))
        (budgets/update (env :db) b)))
    (not-found)))

(defn- delete
  [req]
  (if-let [budget (find-and-auth req ::auth/destroy)]
    (do
      (budgets/delete (env :db) budget)
      (->response))
    (not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/budgets" req (index req))
  (POST "/api/entities/:entity-id/budgets" req (create req))
  (GET "/api/budgets/:id" req (find req))
  (PATCH "/api/budgets/:id" req (update req))
  (DELETE "/api/budgets/:id" req (delete req)))
