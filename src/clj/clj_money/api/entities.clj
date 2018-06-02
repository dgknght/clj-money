(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [ring.util.response :refer [status response header]]
            [cemerick.friend :refer [current-authentication]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize]]
            [clj-money.models.entities :as entities]
            [clj-money.permissions.entities]))

(defn index
  [req]
  (response (entities/select (env :db) (:id (current-authentication)))))

(defn update
  [{:keys [params] :as req}]
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
  (let [entity (authorize (entities/find-by-id (env :db) id) :delete)]
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
