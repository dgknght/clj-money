(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as stow]
            [clj-money.util :refer [update-in-if
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
  (->response (budgets/search (-> params
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
  (-> req
      extract-budget
      (assoc :entity-id (:entity-id params))
      (stow/tag ::models/budget)
      (authorize ::auth/create authenticated)
      budgets/create
      (->response 201)))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [budget (budgets/find-by (+scope {:id (:id params)}
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
     (-> budget
         (merge (extract-budget req))
         budgets/update))
    (not-found)))

(defn- delete
  [req]
  (if-let [budget (find-and-auth req ::auth/destroy)]
    (do
      (budgets/delete budget)
      (->response))
    (not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/budgets" req (index req))
  (POST "/api/entities/:entity-id/budgets" req (create req))
  (GET "/api/budgets/:id" req (find req))
  (PATCH "/api/budgets/:id" req (update req))
  (DELETE "/api/budgets/:id" req (delete req)))
