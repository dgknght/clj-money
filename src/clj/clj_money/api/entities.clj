(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [status response header]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [stowaway.core :as storage]
            [clj-money.util :refer [update-in-if]]
            [clj-money.api :refer [->response
                                   error->response
                                   invalid->response
                                   not-found
                                   log-error]]
            [clj-money.validation :as validation]
            [clj-money.models :as models]
            [clj-money.authorization :refer [authorize +scope] :as authorization]
            [clj-money.models.entities :as entities]
            [clj-money.authorization.entities]))

(defn- index
  [{:keys [authenticated params]}]
  (->response (entities/select (-> params
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
  (let [entity (extract-entity req) ]
    (try
      (let [result (entities/create entity)]
        (if (validation/has-error? result)
          (invalid->response result)
          (->response result 201)))
      (catch Exception e
        (log-error e "Unable to create the entity.")
        (error->response e "Unable to create the entity.")))))

(defn- update
  [{:keys [params body authenticated]}]
  (let [entity (authorize (entities/find (:id params))
                          ::authorization/update
                          authenticated)
        updated (merge entity (-> body
                                  (update-in-if [:settings :monitored-account-ids] set)
                                  (select-keys [:name :settings])))]
    (try
      (let [result (entities/update updated)]
        (if (validation/has-error? result)
          (-> {:message (validation/error-messages result)}
              json/generate-string
              response
              (header "Content-Type" "application/json")
              (status 422))
          (-> result
              json/generate-string
              response
              (header "Content-Type" "application/json")
              (status 200))))
      (catch Exception e
        (->  (if (env :show-error-messages?)
               (.getMessage e)
               "Unable to save the entity.")
            response
            (header "Content-Type" "application/json")
            (status 500))))))

(defn- delete
  [{:keys [params authenticated]}]
  (if-let [entity (first (entities/select (-> params
                                              (select-keys [:id])
                                              (+scope ::models/entity authenticated))))]
    (do
      (entities/delete (authorize entity
                                  ::authorization/destroy
                                  authenticated))
      (->response))
    (not-found)))

(defroutes routes
  (GET "/api/entities" req (index req))
  (PATCH "/api/entities/:id" req (update req))
  (DELETE "/api/entities/:id" req (delete req))
  (POST "/api/entities" req (create req)))
