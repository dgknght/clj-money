(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [status response]]
            [cemerick.friend :refer [current-authentication]]
            [environ.core :refer [env]]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]))

(defn index
  [req]
  (response (entities/select (env :db) (:id (current-authentication)))))

(defn delete
  [{{id :id} :params}]
  (entities/delete (env :db) id)
  {:status 204
   :body []})
