(ns clj-money.api.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [java-time.api :as t]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.budgets :refer [create-items-from-history]]
            [clj-money.entities.transaction-items :as trx-items]
            [clj-money.authorization.budgets]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-id])
      (rename-keys {:entity-id :budget/entity})
      (update-in [:budget/entity] util/->entity-ref)
      (+scope :budget authenticated)))

(defn- index
  [req]
  (api/response
    (entities/select (extract-criteria req)
                   {:sort [[:budget/start-date :desc]]})))

(defn- extract-budget
  [{:keys [params]}]
  (-> params
      (select-keys [:budget/name
                    :budget/start-date
                    :budget/period])
      (update-in-if [:budget/period 1] util/ensure-keyword)
      (update-in-if [:budget/start-date] dates/ensure-local-date)))

(defn- historical-items
  [{:budget/keys [entity period]} start-date]
  (let [end-date (t/plus start-date
                         (dates/period period))]
    (entities/select (util/entity-type
                     #:transaction-item{:transaction/entity entity
                                        :transaction/transaction-date [:between> start-date end-date]
                                        :account/type [:in #{:income :expense}]}
                     :transaction-item)
                   {:select-also [:transaction/transaction-date]})))

(defn- auto-create-items
  [{:budget/keys [period]
    :as budget}
   start-date]
  (let [end-date (t/plus start-date
                         (dates/period period))]
    (->> (historical-items budget start-date)
         (trx-items/realize-accounts)
         (create-items-from-history
           budget
           start-date
           end-date)
         (map #(assoc % :budget-item/budget budget))
         entities/put-many)))

(defn- append-items
  [budget start-date]
  (cond-> budget
    start-date (assoc :budget/items
                      (auto-create-items budget
                                         start-date))))

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> req
      extract-budget
      (assoc :budget/entity {:id (:entity-id params)} )
      (authorize ::auth/create authenticated)
      entities/put ; creating and then updating allows us to skip the transaction lookup if the original budget is not valid
      (append-items (dates/ensure-local-date (:budget/auto-create-start-date params)))
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [budget (-> params
                        (select-keys [:id])
                        (+scope :budget authenticated)
                        (entities/find-by {:include #{:budget/items}}))]
    (authorize budget action authenticated)))

(defn- show
  [req]
  (if-let [budget (find-and-auth req ::auth/show)]
    (api/response budget)
    api/not-found))

(defn- update
  [req]
  (or
    (some-> (find-and-auth req ::auth/update)
            (merge (extract-budget req))
            entities/put
            api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [budget (find-and-auth req ::auth/destroy)]
    (do
      (entities/delete budget)
      (api/response))
    api/not-found))

(def routes
  [["entities/:entity-id/budgets" {:get {:handler index}
                                   :post {:handler create}}]
   ["budgets/:id" {:get {:handler show}
                   :patch {:handler update}
                   :delete {:handler delete}}]])
