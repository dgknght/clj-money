(ns clj-money.middleware
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [ring.util.response :refer [response status header]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [slingshot.slingshot :refer [try+]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]))

(defn- param-name
  [specified-name]
  (if (keyword? specified-name)
    (name specified-name)
    name))

(defmulti ^:private integerize-id-param
  (fn [[k v]]
    (if (re-find #"id$" (param-name k))
      :default
      (type v))))

(defmethod ^:private integerize-id-param :default
  [param]
  param)

(defmethod ^:private integerize-id-param java.lang.String
  [[k v]]
  [k (try
       (Integer. v)
       (catch NumberFormatException e
         v))])

(defn- integerize-id-params
  [params]
  (when params
    (->> params
         (map integerize-id-param)
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
        (-> (json/generate-string {:message "not found"})
            response
            (status 404)
            (header "Content-Type" "application/json"))) ; TODO respond with content type according to accept header
      (catch [:type :clj-money.authorization/no-rules] error-data
        (-> (json/generate-string {:message "no authorization rules"})
            response
            (status 500)
            (header "Content-Type" "application/json")))
      (catch [:type :clj-money.authorization/unauthorized] error-data
        (if (env :show-error-messages?)
          (-> (json/generate-string {:message "unauthorized"})
              response
              (status 403)
              (header "Content-Type" "application/json"))
          (-> (json/generate-string {:message "not found"})
              response
              (status 404)
              (header "Content-Type" "application/json"))))))) ; TODO create a full not-found page
