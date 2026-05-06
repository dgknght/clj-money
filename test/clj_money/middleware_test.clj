(ns clj-money.middleware-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-money.authorization :as authorization]
            [clj-money.api.test-helper :refer [parse-body]]
            [clj-money.middleware :refer [wrap-exceptions
                                          wrap-format]]))

; wrap-infer-entity-type (part of wrap-format) needs a reitit match
; with a :template so it can derive the entity type
(def ^:private mock-request
  {:request-method :get
   :uri "/api/things"
   :headers {"accept" "application/json"}
   :reitit.core/match {:template "/api/things"}})

(defn- invoke-with-error [handler]
  (-> ((-> handler wrap-exceptions wrap-format) mock-request)
      parse-body))

(deftest api-handler-exception-returns-parseable-500
  (testing "plain Exception produces a 500 response the client can read"
    (let [{:keys [status parsed-body]}
          (invoke-with-error (fn [_] (throw (Exception. "something broke"))))]
      (is (= 500 status))
      (is (string? (:message parsed-body))
          "client receives a readable error message")))

  (testing "authorization/no-rules ExceptionInfo produces a 500 the client can read"
    (let [{:keys [status parsed-body]}
          (invoke-with-error
            (fn [_]
              (throw (ex-info "no rules"
                              {:type ::authorization/no-rules}))))]
      (is (= 500 status))
      (is (string? (:message parsed-body))
          "client receives a readable error message"))))
