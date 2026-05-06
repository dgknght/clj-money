(ns clj-money.middleware-test
  (:require [clojure.test :refer [deftest testing is]]
            [muuntaja.core :as muuntaja]
            [clj-money.authorization :as authorization]
            [clj-money.middleware :refer [wrap-exceptions
                                          wrap-format]]))

; wrap-infer-entity-type (part of wrap-format) needs a reitit match
; with a :template so it can derive the entity type
(def ^:private mock-request
  {:request-method :get
   :uri "/api/things"
   :headers {"accept" "application/json"}
   :reitit.core/match {:template "/api/things"}})

(defn- invoke-with-error
  "Wraps handler with wrap-exceptions + wrap-format and makes a request."
  [handler]
  ((-> handler wrap-exceptions wrap-format) mock-request))

(deftest wrap-exceptions-encodes-plain-exception-responses
  (testing "plain Exception produces an encoded 500 response"
    (let [{:keys [status body headers]}
          (invoke-with-error (fn [_] (throw (Exception. "something broke"))))]
      (is (= 500 status))
      (is (not (map? body))
          "body must be encoded; a raw map causes a secondary error in Jetty")
      (is (some? (get headers "Content-Type"))
          "Content-Type is set by muuntaja after encoding")
      (let [parsed (muuntaja/decode "application/json" body)]
        (is (string? (:message parsed))
            "decoded body contains the error message")))))

(deftest wrap-exceptions-encodes-no-rules-exception-responses
  (testing "authorization/no-rules ExceptionInfo produces an encoded 500 response"
    (let [{:keys [status body]}
          (invoke-with-error
            (fn [_]
              (throw (ex-info "no rules"
                              {:type ::authorization/no-rules}))))]
      (is (= 500 status))
      (is (not (map? body))
          "body must be encoded; a raw map causes a secondary error in Jetty"))))
