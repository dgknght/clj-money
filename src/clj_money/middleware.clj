(ns clj-money.middleware
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [slingshot.slingshot :refer [try+]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]))

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


(def ^:private param-models
  [{:key :entity-id
    :lookup-fn #(entities/find-by-id (env :db) %)
    :target-key :entity}
   {:key :account-id
    :lookup-fn #(accounts/find-by-id (env :db) %)
    :target-key :account}])

(defn- lookup-models
  [params]
  (->> param-models
       (map (fn [{:keys [key lookup-fn target-key]}]
              (when-let [id (key params)]
                [target-key (lookup-fn id)])))
       (filter (fn [[k v]] v))
       (into {})))

(defn wrap-models
  "Adds :entity to the params if :entity-id is present"
  [handler]
  (fn [request]
    (handler (update-in request [:params] #(merge % (lookup-models %))))))

(defn wrap-exception-handling
  [handler]
  (fn [request]
    (try+
      (handler request)
      (catch [:type :clj-money.models/not-found] error-data
        {:status 404
         :headers {}
         :body "not found"})
      (catch [:type :clj-money.authorization/unauthorized] error-data
        {:status 404
         :headers {}
         :body "not found"})))) ; TODO create a full not-found page
