(ns clj-money.otel.web
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.otel :refer [with-tracing
                                    set-attribute]]))

(defn wrap-otel
  [handler]
  (fn [{:keys [request-method request-url]
        :reitit.core/keys [match]
        :as req}]
    (with-tracing
      [span (format "HTTP %s %s" request-method (:template match))]

      (set-attribute span "http" "request" "url" request-url)
      (doseq [[k v] (:path-params match)]
        (set-attribute span "http" "request" "path-params" k v))

      (let [{:as res :keys [status]} (handler req)]
        (set-attribute span "http" "response" "status-code" status)
        res))))
