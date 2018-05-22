(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [ring.util.response :refer [status response header]]
            [cemerick.friend :refer [current-authentication]]
            [environ.core :refer [env]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize]]
            [clj-money.models.entities :as entities]
            [clj-money.permissions.entities]))

(defn index
  [req]
  (response (entities/select (env :db) (:id (current-authentication)))))

(defn update
  [{{id :id} :params}]
  (let [entity (authorize :update (entities/find-by-id (env :db) id))]
    (try
      (entities/update (env :db) entity)
      (-> []
          response
          (status 204))
      (catch Exception e
        (->  (if (env :show-error-messages?)
               (.getMessage e)
               "Unable to save the entity.")
            response
            (header "Content-Type" "application/json")
            (status 500))))))

(defn delete
  [{{id :id} :params}]
  (let [entity (authorize :delete (entities/find-by-id (env :db) id))]
    (try
    (entities/delete (env :db) (:id entity))
    (catch Exception e
      (-> (if (env :show-error-messages?)
            (.getMessage e)
            "Unable to delete the entity.")
          response
          (header "Content-Type" "application/json")
          (status 500))))
    {:status 204
     :body []}))
