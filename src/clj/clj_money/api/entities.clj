(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [status response header]]
            [cemerick.friend :refer [current-authentication]]
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

(defn index
  [_]
  (response (entities/select (env :db) (:id (current-authentication)))))

(defn create
  [{:keys [params]}]
  (let [entity (-> params
                   (select-keys [:name :settings])
                   (assoc :user-id (:id (current-authentication)))
                   (tag-resource :entity))]
    (try
      (let [result (entities/create (env :db) entity)]
        (if (validation/has-error? result)
          (invalid->response result)
          (->response result 201)))
      (catch Exception e
        (log-error e "Unable to create the entity.")
        (error->response e "Unable to create the entity.")))))

(defn update
  [{:keys [params]}]
  (let [entity (authorize (entities/find-by-id (env :db) (:id params)) :update)
        updated (merge entity (select-keys params [:name :settings]))]
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

(defn delete
  [{{id :id} :params}]
  (delete-resource id entities/find-by-id entities/delete))
