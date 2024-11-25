(ns clj-money.api.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.budgets :refer [create-items-from-history]]
            [clj-money.authorization.budgets]))

(defn- index
  [{:keys [params authenticated]}]
  (api/response
    (models/select (-> params
                       (select-keys [:entity-id])
                       (rename-keys {:entity-id :budget/entity-id})
                       (+scope :budget authenticated))
                   {:sort [[:budget/start-date :desc]]})))

(defn- prepare-item
  [item]
  (-> item
      (update-in-if [:budget-item/periods] #(map bigdec %))
      (update-in-if [:budget-item/spec :start-date] dates/unserialize-local-date)))

(defn- extract-budget
  [{:keys [body]}]
  (-> body
      (select-keys [:budget/name
                    :budget/start-date
                    :budget/period
                    :budget/period-count
                    :budget/items])
      (update-in-if [:budget/items] #(map prepare-item %))
      (update-in-if [:budget/period] keyword)
      (update-in-if [:budget/start-date] dates/unserialize-local-date)))

(defn- auto-create-items
  [{:budget/keys [entity period period-count] :as budget} start-date]
  (let [end-date (t/plus start-date
                         ((case period
                            :month t/months
                            :year t/years
                            :week t/weeks)
                          period-count))
        items (models/select #:transaction-item{:transaction/entity entity
                                                :transaction/transaction-date [:between> start-date end-date]
                                                :account/type [:in #{:income :expense}]})]
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
        (assoc :budget/items (auto-create-items
                               budget
                               (dates/unserialize-local-date start-date)))
        models/put)
    budget))

(defn- create
  [{:keys [authenticated params body] :as req}]
  (-> req
      extract-budget
      (assoc :budget/entity {:id (:entity-id params)} )
      (authorize ::auth/create authenticated)
      models/put ; creating and then updating allows us to skip the transaction lookup if the original budget is not valid
      (append-items (:budget/auto-create-start-date body))
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [budget (-> params
                        (select-keys [:id])
                        (+scope :budget authenticated)
                        models/find-by)]
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
        models/put
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [budget (find-and-auth req ::auth/destroy)]
    (do
      (models/delete budget)
      (api/response))
    api/not-found))

(def routes
  [["entities/:entity-id/budgets" {:get {:handler index}
                                   :post {:handler create}}]
   ["budgets/:id" {:get {:handler show}
                   :patch {:handler update}
                   :delete {:handler delete}}]])
