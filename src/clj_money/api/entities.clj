(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [stowaway.core :as storage]
            [dgknght.app-lib.core :refer [update-in-if]]
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
  [{:keys [body authenticated]}]
  (-> body
      (select-keys [:name :settings])
      (assoc :user-id (:id authenticated))
      (update-in-if [:settings :inventory-method] keyword)
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
  [{:keys [body] :as req}]
  (if-let [entity (find-and-auth req ::authorization/update)]
    (-> entity
        (merge (-> body
                   (update-in-if [:settings :monitored-account-ids] set)
                   (select-keys [:name :settings])))
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

(defroutes routes
  (GET "/api/entities" req (index req))
  (PATCH "/api/entities/:id" req (update req))
  (DELETE "/api/entities/:id" req (delete req))
  (POST "/api/entities" req (create req)))
