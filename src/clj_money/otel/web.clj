(ns clj-money.otel.web
  (:require [clj-money.otel :refer [with-tracing
                                    set-attribute]]))

(defn- request-url
  [{:keys [uri query-string]}]
  (cond-> uri
    query-string (str "?" query-string)))

(defn wrap-otel
  [handler]
  (fn [{:keys [request-method]
        :reitit.core/keys [match]
        :as req}]
    (with-tracing
      [span (format "HTTP %s %s" request-method (:template match))]

      (set-attribute span "http" "request" "url" (request-url req))
      (doseq [[k v] (:path-params match)]
        (set-attribute span "http" "request" "path-params" k v))

      (let [{:as res :keys [status]} (handler req)]
        (set-attribute span "http" "response" "status-code" status)
        res))))
