(ns clj-money.middleware
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk]]
            [clojure.string :as string]
            [ring.util.response :refer [response status header]]
            [muuntaja.middleware :refer [wrap-format-negotiate
                                         wrap-format-request
                                         wrap-format-response]]
            [muuntaja.core :as muuntaja]
            [camel-snake-kebab.core :refer [->camelCaseKeyword]]
            [dgknght.app-lib.core :refer [uuid]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.inflection :refer [singular]]
            [clj-money.authorization :as authorization]
            [clj-money.entities :as entities]
            [clj-money.api :refer [log-error]]))

(defn- param-name
  [specified-name]
  (if (keyword? specified-name)
    (name specified-name)
    specified-name))

(def ^:private long-pattern #"\A\d+\z")
(def ^:private uuid-pattern #"\A[a-f0-9]{8}(-[a-f0-9]{4}){3}-[a-f0-9]{12}\z")

(defn- parse-id
  [v]
  (cond
    (string? v)
    (cond
      (re-find long-pattern v) (parse-long v)
      (re-find uuid-pattern v) (uuid v)
      :else v)

    (coll? v)
    (mapv parse-id v)

    :else v))

(defn- ends-with-id?
  [s]
  (boolean (re-find #"id$" (param-name s))))

(def ^:private id-key?
  (every-pred (some-fn string?
                       keyword?)
              ends-with-id?))

(def ^:private id-param?
  (every-pred map-entry?
              (comp id-key? key)))

(defn- parse-id-params
  [params]
  (when params
    (postwalk (fn [x]
                (if (id-param? x)
                  (update-in x [1] parse-id)
                  x))
              params)))

(defn wrap-parse-id-params
  "Finds ID parameters and turns them into integers"
  [handler]
  (fn [request]
    (handler (update-in request [:params] parse-id-params))))

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

(defmethod handle-exception ::entities/not-found
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
    (log/errorf e "Unexpected ExceptionInfo was encountered while handling the web request: %s" (pr-str details))
    (log/error e "Unexpected ExceptionInfo was encountered while handling the web request."))
  api/internal-server-error)

; TODO: Move this to the api namespace
(defn wrap-exceptions
  [handler]
  (fn [request]
    (try
     (handler request)
     (catch clojure.lang.ExceptionInfo e
       (log-error e "unexpected clojure error")
       (handle-exception e))
     (catch Exception e
       (log-error e "unexpected error")
       ; TODO: only do this if in local development mode
       (-> {:message (str "unexpected error: " (or (.getMessage e)
                                                   (.getClass e)))}
           response
           (status 500)
           (header "Content-Type" "application/json"))))))

(defmulti ^:private conventionalize-content
  (fn [_content content-type] content-type))

(defmethod conventionalize-content :default
  [content _]
  content)

(defmethod conventionalize-content "application/json"
  [content _]
  (postwalk (fn [x]
              (if (map-entry? x)
                (update-in x [0] (comp ->camelCaseKeyword
                                       name))
                x))
            content))

(defn- conventionalize-response
  [res content-type]
  (update-in res [:body] #(conventionalize-content % content-type)))

(defn- wrap-conventionalize-response
  [handler]
  (fn [{:as req :muuntaja/keys [response]}]
    (-> req
        handler
        (conventionalize-response (:format response)))))

(defn- wrap-infer-entity-type
  [handler]
  (fn [{:as req :reitit.core/keys [match]}]
    (let [final-segment (->> (string/split (:template match) #"/")
                             (remove #{"api"})
                             (remove #(re-find #"^:" %))
                             last)]
      (-> req
          (assoc :entity-type (singular final-segment))
          handler))))

(defmulti ^:private apply-keyword-namespaces (comp :muuntaja/request :format))

(defmethod apply-keyword-namespaces :default [req] req)

(defmethod apply-keyword-namespaces "application/json"
  [{:as req :keys [entity-type]}]
  (update-in req
             [:body-params]
             (partial postwalk
                      (fn [x]
                        (if (map-entry? x)
                          (update-in x [0] #(keyword entity-type
                                                     (name %)))
                          x)))))

(defn- wrap-apply-keyword-namespaces
  [handler]
  (fn [req]
    (-> req apply-keyword-namespaces handler)))

(def wrap-format
  (comp #(wrap-format-negotiate % (assoc muuntaja/default-options :default-format "application/edn"))
        wrap-format-request
        wrap-infer-entity-type
        wrap-apply-keyword-namespaces
        wrap-format-response
        wrap-conventionalize-response))
