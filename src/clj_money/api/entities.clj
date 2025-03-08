(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.core :as storage]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.authorization :refer [authorize +scope] :as authorization]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.authorization.entities]))

(defn- index
  [{:keys [authenticated params]}]
  (api/response
    (entities/select (-> params
                         (select-keys [:name])
                         (+scope ::models/entity authenticated)))))

(defn- extract-entity
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:name :settings])
      (assoc :user-id (:id authenticated))
      (storage/tag ::models/entity)))

(defn- create
  [req]
  (-> req
      extract-entity
      entities/create
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope ::models/entity authenticated)
          entities/find-by
          (authorize action authenticated)))

(defn- update
  [{:keys [params] :as req}]
  (if-let [entity (find-and-auth req ::authorization/update)]
    (-> entity
        (merge (select-keys params [:name :settings]))
        entities/update
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [entity (find-and-auth req ::authorization/destroy)]
    (do
      (entities/delete entity)
      (api/response))
    api/not-found))

(def routes
  ["entities"
   ["" {:get {:handler index}
        :post {:handler create}}]
   ["/:id" {:patch {:handler update}
            :delete {:handler delete}}]])
