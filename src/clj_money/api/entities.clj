(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.authorization :refer [authorize +scope] :as authorization]
            [clj-money.models :as models]
            [clj-money.authorization.entities]))

(defn- index
  [{:keys [authenticated params]}]
  (api/response
    (models/select (-> params
                       (select-keys [:name])
                       (+scope :entity authenticated))
                   {:sort [:entity/name]})))

(defn- extract-entity
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity/name
                    :entity/settings])
      (update-in [:entity/user] (fnil identity authenticated))))

(defn- create
  [req]
  (-> req
      extract-entity
      models/put
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (util/model-type :entity)
          (+scope :entity authenticated)
          models/find-by
          (authorize action authenticated)))

(defn- update
  [req]
  (if-let [entity (find-and-auth req ::authorization/update)]
    (-> entity
        (merge (extract-entity req))
        models/put
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [entity (find-and-auth req ::authorization/destroy)]
    (do
      (models/delete entity)
      (api/response))
    api/not-found))

(def routes
  ["entities"
   ["" {:get {:handler index}
        :post {:handler create}}]
   ["/:id" {:patch {:handler update}
            :delete {:handler delete}}]])
