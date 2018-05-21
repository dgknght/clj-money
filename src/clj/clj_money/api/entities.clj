(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [ring.util.response :refer [status response]]
            [cemerick.friend :refer [current-authentication]]
            [environ.core :refer [env]]
            [clj-money.validation :as validation]
            [clj-money.authorization :refer [authorize]]
            [clj-money.models.entities :as entities]))

(defn index
  [req]
  (response (entities/select (env :db) (:id (current-authentication)))))

(defn update
  [{:keys [params]}]
  (let [entity (authorize :edit (:entity params))]
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
            (status 500))))))

(defn delete
  [{{id :id} :params}]
  (entities/delete (env :db) id)
  {:status 204
   :body []})
