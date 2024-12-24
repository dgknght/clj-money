(ns clj-money.middleware
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [ring.util.response :refer [response status header]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.validation :as v]
            [clj-money.authorization :as authorization]
            [clj-money.models :as models]
            [clj-money.api :refer [log-error]]))

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
    (if (re-find #"^\d+$" value)
      (Integer. value)
      value)
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

(defmulti handle-exception
  (fn [e]
    (when-let [data (ex-data e)]
      (if (::v/errors data)
        :validation
        (:type data)))))

(defmethod handle-exception ::authorization/unauthorized
  [e]
  (if (authorization/opaque? (ex-data e))
    api/not-found
    api/forbidden))

(defmethod handle-exception ::authorization/not-found
  [_]
  api/not-found)

(defmethod handle-exception ::authorization/no-rules
  [_]
  (-> {:message "no authorization rules"}
      response
      (status 500)
      (header "Content-Type" "application/json")))

(defmethod handle-exception ::models/not-found
  [_]
  api/not-found)

(defmethod handle-exception :validation
  [e]
  (-> e
      ex-data
      ::v/errors
      (api/response 400)))

(defmethod handle-exception :default
  [e]
  (if-let [details (ex-data e)]
    (log/error e "Unexpected ExceptionInfo was while hanlding the web request: %s" (pr-str details))
    (log/error e "Unexpected ExceptionInfo was while hanlding the web request."))
  api/internal-server-error)

; TODO: Move this to the api namespace
(defn wrap-exceptions
  [handler]
  (fn [request]
    (try
     (handler request)
     (catch clojure.lang.ExceptionInfo e
       (handle-exception e))
     (catch Exception e
       (log-error e "unexpected error")
       ; TODO: only do this if in local development mode
       (-> {:message (str "unexpected error: " (or (.getMessage e)
                                                   (.getClass e)))}
           response
           (status 500)
           (header "Content-Type" "application/json"))))))
