(ns clj-money.api.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [java-time.api :as t]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.budgets :refer [create-items-from-history]]
            [clj-money.models.transaction-items :as trx-items]
            [clj-money.authorization.budgets]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-id])
      (rename-keys {:entity-id :budget/entity-id})
      (+scope :budget authenticated)))

(defn- index
  [req]
  (api/response
    (models/select (extract-criteria req)
                   {:sort [[:budget/start-date :desc]]})))

(defn- extract-budget
  [{:keys [params]}]
  (-> params
      (select-keys [:budget/name
                    :budget/start-date
                    :budget/period
                    :budget/period-count
                    :budget/items])
      (update-in-if [:budget/items] (fn [items]
                                      (mapv #(update-in % [:budget-item/periods] vec)
                                            items)))))

(defn- historical-items
  [{:budget/keys [entity period period-count]} start-date]
  (let [end-date (t/plus start-date
                         ((case period
                            :month t/months
                            :year t/years
                            :week t/weeks)
                          period-count))]
    (models/select (util/model-type
                     #:transaction-item{:transaction/entity entity
                                        :transaction/transaction-date [:between> start-date end-date]
                                        :account/type [:in #{:income :expense}]}
                     :transaction-item))))

(defn- auto-create-items
  [{:budget/keys [period period-count] :as budget} start-date]
  (let [end-date (t/plus start-date
                         ((case period
                            :month t/months
                            :year t/years
                            :week t/weeks)
                          period-count))]
    (->> (historical-items budget start-date)
         (trx-items/realize-accounts)
         (create-items-from-history
           budget
           start-date
           end-date))))

(defn- assoc-auto-created-items
  [budget start-date]
  (if-let [items (seq
                   (auto-create-items
                     budget
                     start-date))]
    (assoc budget
           :budget/items
           (vec items))
    budget))

(defn- append-items
  [budget start-date]
  (if start-date
    (-> budget
        (assoc-auto-created-items start-date)
        models/put)
    budget))

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> req
      extract-budget
      (assoc :budget/entity {:id (:entity-id params)} )
      (authorize ::auth/create authenticated)
      models/put ; creating and then updating allows us to skip the transaction lookup if the original budget is not valid
      (append-items (:auto-create-start-date params))
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
