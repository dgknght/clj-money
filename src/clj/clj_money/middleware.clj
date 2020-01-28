(ns clj-money.middleware
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [response status header]]
            [environ.core :refer [env]]
            [slingshot.slingshot :refer [try+]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]))

(defn- param-name
  [specified-name]
  (if (keyword? specified-name)
    (name specified-name)
    specified-name))

(defmulti ^:private integerize type)

(defmethod ^:private integerize :default [v] v)

(defmethod ^:private integerize java.lang.String
  [value]
  (try
    (Integer. value)
    (catch NumberFormatException _
      value)))

(defn- id-key?
  [k]
  (boolean (re-find #"id$" (param-name k))))

(defn- integerize-id-params
  [params]
  (when params
    (->> params
         (map (fn [[k v]]
                [k (if (id-key? k)
                     (integerize v)
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
       (filter second)
       (into {})))

(defn wrap-models
  "Adds :entity to the params if :entity-id is present"
  [handler]
  (fn [request]
    (handler (update-in request [:params] #(merge % (lookup-models %))))))

(defn wrap-exceptions
  [handler]
  (fn [request]
    (try+
      (handler request)
      (catch [:type :clj-money.models/not-found] error-data
        (-> {:message "not found"}
            response
            (status 404)
            (header "Content-Type" "application/json")))
      (catch [:type :clj-money.authorization/no-rules] error-data
        (-> {:message "no authorization rules"}
            response
            (status 500)
            (header "Content-Type" "application/json")))
      (catch [:type :clj-money.authorization/unauthorized] error-data
        (-> {:message "not found"}
            response
            (status 404)
            (header "Content-Type" "application/json"))))))
