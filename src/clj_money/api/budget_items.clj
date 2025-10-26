(ns clj-money.api.budget-items
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization.budget-items]))

(defn- extract-criteria
  [{:keys [params]}]
  (-> params
      (select-keys [:budget-id])
      (rename-keys {:budget-id :budget-item/budget})
      (update-in [:budget-item/budget] util/->entity-ref)))

(defn- index
  [{:keys [authenticated] :as req}]
  (-> req
      extract-criteria
      (util/pp-> ::criteria)
      (+scope :budget-item authenticated)
      (util/pp-> ::scoped)
      entities/select
      (util/pp-> ::selected)
      api/response))

(defn- extract-item
  [{:keys [params]}]
  (-> params
      (select-keys [:budget-item/periods ; TODO: use list of attributes derived from schema instead
                    :budget-item/account
                    :budget-item/spec])
      (update-in [:budget-item/periods] (partial mapv bigdec))))

(defn- create
  [{:keys [authenticated params] :as req}]
  (if-let [budget (authorize
                    (entities/find-by
                      (+scope (util/entity-type {:id (:budget-id params)}
                                               :budget)
                              :budget
                              authenticated))
                    ::auth/update
                    authenticated)]
    (-> req
        extract-item
        (assoc :budget-item/budget budget)
        entities/put
        api/creation-response)
    api/not-found))

(defn- find-item
  [{:keys [params authenticated]}]
  (entities/find-by (+scope (util/entity-type {:id (:id params)}
                                           :budget-item)
                          :budget-item
                          authenticated)))

(defn- update
  [{:as req :keys [authenticated]}]
  (or (some-> (find-item req)
              (authorize ::auth/update authenticated)
              (merge (extract-item req))
              entities/put
              api/response)
      api/not-found))

(defn- delete
  [{:as req :keys [authenticated]}]
  (if-let [item (find-item req)]
    (do
      (-> item
          (authorize ::auth/destroy authenticated)
          entities/delete)
      api/no-content)
    api/not-found))

(def routes
  [["budgets/:budget-id/items" {:get {:handler index}
                                :post {:handler create}}]
   ["budget-items/:id" {;:get {:handler show}
                        :patch {:handler update}
                        :delete {:handler delete}}]])
