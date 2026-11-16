(ns clj-money.otel.web-test
  (:require [clojure.test :refer [deftest is]]
            [reitit.ring :as ring]
            [clj-money.otel-helpers :refer [capture]]
            [clj-money.otel.web :refer [wrap-otel]]))

(def ^:private app
  (ring/ring-handler
    (ring/router ["/" {:middleware [wrap-otel]}
                  ["accounts/:id" {:get (fn [_] {:status 200 :body "OK"})}]
                  ["failures" {:get (fn [_] (throw (ex-info "Boom" {})))}]])))

(deftest trace-a-request
  (let [{:keys [result] [span :as spans] :spans}
        (capture #(app {:request-method :get
                        :uri "/accounts/101"
                        :query-string "as-of=2015-01-01"}))]
    (is (= 200 (:status result))
        "The response is returned")
    (is (= 1 (count spans))
        "One span is created")
    (is (= "HTTP :get /accounts/:id" (:name span))
        "The span is named for the method and the route template")
    (is (= {"http.request.url" "/accounts/101?as-of=2015-01-01"
            "http.request.path_params.id" "101"
            "http.response.status_code" 200}
           (:attributes span))
        "The request and the response are added to the span")))

(deftest trace-a-failed-request
  (let [{:keys [exception] [span] :spans}
        (capture #(app {:request-method :get
                        :uri "/failures"}))]
    (is (= "Boom" (ex-message exception))
        "The exception is rethrown")
    (is (= :error (:status span))
        "The span status is set to error")
    (is (= {"http.request.url" "/failures"}
           (:attributes span))
        "The request is added to the span")))
