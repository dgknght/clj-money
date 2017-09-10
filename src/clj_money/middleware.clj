(ns clj-money.middleware
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-money.models.entities :as entities])
  (:import clj_money.NotAuthorizedException))

(defn- integerize-id-params
  [params]
  (when params
    (->> params
         (map (fn [[k v]]
                [k (if (re-matches #"^(.+-)?id$" (if (keyword? k)
                                               (name k)
                                               k))
                     (try
                       (Integer. v)
                       (catch NumberFormatException e
                         v))
                     v)]))
         (into {}))))

(defn wrap-integer-id-params
  "Finds ID parameters and turns them into integers"
  [handler]
  (fn [request]
    (handler (update-in request [:params] integerize-id-params))))


(defn- lookup-entity
  [params]
  (cond-> params
    (:entity-id params)
    (assoc :entity (entities/find-by-id (env :db) (:entity-id params)))))

(defn wrap-entity
  "Adds :entity to the params if :entity-id is present"
  [handler]
  (fn [request]
    (handler (update-in request [:params] lookup-entity))))

(defn wrap-exception-handling
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch NotAuthorizedException e
        {:status 403
         :headers {}
         :body "forbidden"}))))
