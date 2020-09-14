(ns clj-money.middleware
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [response status header]]
            [slingshot.slingshot :refer [try+]]
            [clj-money.api :as api]))

(defn- param-name
  [specified-name]
  (if (keyword? specified-name)
    (name specified-name)
    specified-name))

(defmulti ^:private integerize
  (fn [v]
    (cond
      (string? v) :string
      (coll? v) :collection)))

(defmethod ^:private integerize :default [v] v)

(defmethod ^:private integerize :string
  [value]
  (try
    (Integer. value)
    (catch NumberFormatException _
      value)))

(defmethod ^:private integerize :collection
  [values]
  (map integerize values))

(defn- id-key?
  [k]
  (boolean (re-find #"id$" (param-name k))))

(defn- integerize-id-param
  [[k v]]
  [k (if (id-key? k)
       (integerize v)
       v)])

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

(defn- normalize-collection-param
  [param]
  (update-in param [0] #(if-let [match (re-find #"^(.+)\[\]$" %)]
                           (keyword (second match))
                           %)))

(defn- normalize-collection-params
  [params]
  (when params
    (->> params
         (map normalize-collection-param)
         (into {}))))

(defn wrap-collection-params
  "Finds params for collection values normalizes the keys"
  [handler]
  (fn [req]
    (handler (update-in req [:params] normalize-collection-params))))

; TODO: Move this to the api namespace
(defn wrap-exceptions
  [handler]
  (fn [request]
    (try+
      (handler request)
      (catch [:type :clj-money.models/not-found] error-data
        (api/not-found))
      (catch [:type :clj-money.authorization/no-rules] error-data
        (-> {:message "no authorization rules"}
            response
            (status 500)
            (header "Content-Type" "application/json")))
      (catch [:type :clj-money.authorization/unauthorized] error-data
        (api/not-found))
      (catch Exception e
        (api/log-error e "unexpected error")
        (-> {:message (str "unexpected error: " (or (.getMessage e)
                                                    (.getClass e)))}
            response
            (status 500)
            (header "Content-Type" "application/json"))))))
