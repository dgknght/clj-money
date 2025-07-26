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
            [clj-money.models :as models]
            [clj-money.budgets :refer [create-items-from-history]]
            [clj-money.models.transaction-items :as trx-items]
            [clj-money.authorization.budgets]))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity])
      (rename-keys {:entity :budget/entity})
      (update-in-if [:budget/entity] #(vector :id %))
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
                    :budget/period])))

(defn- historical-items
  [{:budget/keys [entity period]} start-date]
  (let [end-date (t/plus start-date
                         (dates/period period))]
    (models/select (util/model-type
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
         models/put-many)))

(defn- append-items
  [budget start-date]
  (cond-> budget
    start-date (assoc :budget/items
                      (auto-create-items budget
                                         start-date))))

; I'm not sure this is the best way to handle this, but it's working for now anyway
(defn- parse-entity-id
  [x]
  (if (string? x)
    (parse-long x)
    x))

(defn- create
  [{:keys [authenticated params] :as req}]
  (-> req
      extract-budget
      (assoc :budget/entity {:id (parse-entity-id (:entity-id params))} )
      (authorize ::auth/create authenticated)
      models/put ; creating and then updating allows us to skip the transaction lookup if the original budget is not valid
      (append-items (:budget/auto-create-start-date params))
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (when-let [budget (-> params
                        (select-keys [:id])
                        (+scope :budget authenticated)
                        (models/find-by {:include #{:budget/items}}))]
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
