(ns clj-money.api.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as stow]
            [dgknght.app-lib.core :refer [update-in-if]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.budgets :refer [create-items-from-history]]
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
      (update-in-if [:spec :start-date] dates/unserialize-local-date)))

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
      (update-in-if [:start-date] dates/unserialize-local-date)))

(defn- auto-create-items
  [{:keys [entity-id period period-count] :as budget} start-date]
  (let [end-date (t/plus start-date
                         ((case period
                            :month t/months
                            :year t/years
                            :week t/weeks)
                          period-count))
        items (trans/search-items {[:transaction :entity-id] entity-id
                                   [:account :type] [:in #{:income :expense}]
                                   :transaction-date [:between> start-date end-date]})]
    (create-items-from-history
      budget
      start-date
      end-date
      items)))

(defn- append-items
  [budget start-date]
  (if (and (not (v/has-error? budget))
           start-date)
    (-> budget
        (assoc :items (auto-create-items
                        budget
                        (dates/unserialize-local-date start-date)))
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
      (append-items (:auto-create-start-date body))
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [budget (budgets/find-by (+scope {:id (:id params)}
                                             ::models/budget
                                             authenticated))]
    (authorize budget action authenticated)))

(defn- show
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

(def routes
  [["entities/:entity-id/budgets" {:get {:handler index}
                                   :post {:handler create}}]
   ["budgets/:id" {:get {:handler show}
                   :patch {:handler update}
                   :delete {:handler delete}}]])
