(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [status response header]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-money.api :refer [->response
                                   error->response
                                   invalid->response
                                   delete-resource
                                   log-error]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize
                                             tag-resource]]
            [clj-money.models.entities :as entities]
            [clj-money.permissions.entities]))

(defn- index
  [{:keys [authenticated]}]
  (->response (entities/select (env :db) {:user-id (:id authenticated)})))

(defn- create
  [{:keys [body authenticated]}]
  (let [entity (-> body
                   (select-keys [:name :settings])
                   (assoc :user-id (:id authenticated))
                   (tag-resource :entity))]
    (try
      (let [result (entities/create (env :db) entity)]
        (if (validation/has-error? result)
          (invalid->response result)
          (->response result 201)))
      (catch Exception e
        (log-error e "Unable to create the entity.")
        (error->response e "Unable to create the entity.")))))

(defn- update
  [{:keys [params body authenticated]}]
  (let [entity (authorize (entities/find-by-id (env :db) (:id params))
                          :update
                          authenticated)
        updated (merge entity (select-keys body [:name :settings]))]
    (try
      (let [result (entities/update (env :db) updated)]
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
  (delete-resource (:id params)
                   authenticated
                   entities/find-by-id
                   entities/delete))

(defroutes routes
  (GET "/api/entities" req (index req))
  (PATCH "/api/entities/:id" req (update req))
  (DELETE "/api/entities/:id" req (delete req))
  (POST "/api/entities" req (create req)))
