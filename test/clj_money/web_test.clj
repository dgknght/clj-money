(ns clj-money.web-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [dgknght.app-lib.test-assertions]
            [clj-money.web.server :refer [app]]))

(deftest fetch-the-main-page
  (let [logs (atom [])]
    (with-redefs [log/log* (fn [& args]
                             (swap! logs conj args))]
      (let [res (app (req/request :get "/"))]
        (is (http-success? res)
            "The request is sucessful"))
      (let [[[_ l1 _ m1]
             [_ l2 _ m2]
             [_ l3 _ m3]
             [_ l4 _ m4]
             :as ls] @logs]
        (is (= 4 (count ls))
            "Two request log entries and two response log entries are written")
        (is (= [:info "Request :get \"/\""] [l1 m1])
            "The request basics are logged at the info level")
        (is (= :debug l2)
            "The request details are logged at the debug level")
        (is (re-find #"^Request details :get" m2)
            "The request details are logged at the debug level")
        (is (= [:info "Response :get \"/\" -> 200"] [l3 m3])
            "The response basics are logged at the info level")
        (is (= :debug l4)
            "The response details are logged at the debug level")
        (is (re-find #"^Response details :get" m4)
            "The response details are logged at the debug level")))))

(deftest an-unknown-url-returns-a-404
  (is (http-not-found? (app (req/request :get "/not-a-valid-path")))))

(deftest images-are-returned
  (is (http-success? (app (req/request :get "/images/logo.svg")))))
