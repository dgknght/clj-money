(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as stow]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-time.core :as t]
            [dgknght.app-lib.web :refer [unserialize-date]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
            [clj-money.transactions :refer [summarize-items]]
            [clj-money.models.transactions :as trans]
            [clj-money.models.budgets :as budgets]
            [clj-money.authorization.budgets]))

(defn- index
  [{:keys [params authenticated]}]
  (api/response
    (budgets/search (-> params
                        (select-keys [:entity-id])
                        (+scope ::models/budget authenticated))
                    {:sort [[:start-date :desc]]})))
(defn- prepare-item
  [item]
  (-> item
      (update-in-if [:periods] #(map bigdec %))
      (update-in-if [:spec :start-date] unserialize-date)))

(defn- extract-budget
  [{:keys [body]}]
  (-> body
      (select-keys [:name
                    :start-date
                    :period
                    :period-count
                    :items])
      (update-in-if [:items] #(map prepare-item %))
      (update-in-if [:period] keyword)
      (update-in-if [:start-date] unserialize-date)))

(defn- ->budget-item
  [[account-id tran-items]
   {:keys [period]}
   start-date
   end-date]
  {:account-id account-id
   :periods (->> tran-items
                 (summarize-items {:interval-type period
                                   :interval-count 1
                                   :start-date start-date
                                   :end-date end-date})
                 (map :quantity))})

(defn- create-items-from-history
  [{:keys [entity-id period period-count] :as budget} start-date]
  (let [end-date (t/minus
                   (t/plus start-date
                           ((case period
                              :month t/months
                              :year t/years
                              :week t/weeks)
                            period-count))
                   (t/days 1))]
    (->> (trans/search-items {[:transaction :entity-id] entity-id
                              [:account :type] [:in #{:income :expense}]
                              :transaction-date [:between start-date end-date]})
         (group-by :account-id)
         (map #(->budget-item % budget start-date end-date)))))

(defn- auto-create-items
  [budget start-date]
  (if (and (not (v/has-error? budget))
           start-date)
    (-> budget
        (assoc :items (create-items-from-history
                        budget
                        start-date))
        budgets/update)
    budget))

(defn- create
  [{:keys [authenticated params body] :as req}]
  (-> req
      extract-budget
      (assoc :entity-id (:entity-id params))
      (stow/tag ::models/budget)
      (authorize ::auth/create authenticated)
      budgets/create ; creating and then updating allows us to skip the transaction lookup if the original budget is not valid
      (auto-create-items (-> body
                             :auto-create-start-date
                             unserialize-date))
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [budget (budgets/find-by (+scope {:id (:id params)}
                                             ::models/budget
                                             authenticated))]
    (authorize budget action authenticated)))

(defn- find
  [req]
  (if-let [budget (find-and-auth req ::auth/show)]
    (api/response budget)
    api/not-found))

(defn- update
  [req]
  (if-let [budget (find-and-auth req ::auth/update)]
    (-> budget
        (merge (extract-budget req))
        budgets/update
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [budget (find-and-auth req ::auth/destroy)]
    (do
      (budgets/delete budget)
      (api/response))
    api/not-found))

(defroutes routes
  (GET "/api/entities/:entity-id/budgets" req (index req))
  (POST "/api/entities/:entity-id/budgets" req (create req))
  (GET "/api/budgets/:id" req (find req))
  (PATCH "/api/budgets/:id" req (update req))
  (DELETE "/api/budgets/:id" req (delete req)))
