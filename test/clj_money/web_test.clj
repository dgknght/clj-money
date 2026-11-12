(ns clj-money.web-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [hickory.core :as hick]
            [hickory.select :as hsel]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.test-handler :refer [app]])
  (:import java.nio.file.Files
           java.security.MessageDigest
           java.util.HexFormat))

(use-fixtures :each reset-db)

(deftest fetch-the-main-page
  (let [logs (atom [])]
    (with-redefs [log/log* (fn [& args]
                             (swap! logs conj args))]
      (let [res (app (req/request :get "/"))]
        (is (http-success? res)
            "The request is sucessful"))
      (let [[[_ l1 _ m1]
             [_ l2 _ m2]
             :as ls] (filter #(= :info (second %)) @logs)]
        (is (= 2 (count ls))
            "Two request log entries and two response log entries are written")
        (is (= [:info "Request :get \"/\""] [l1 m1])
            "The request basics are logged at the info level")
        (is (= [:info "Response :get \"/\" -> 200"] [l2 m2])
            "The response basics are logged at the info level")))))

(deftest version-the-static-asset-urls-in-the-main-page
  (let [res (app (req/request :get "/"))
        expected (-> (MessageDigest/getInstance "SHA-256")
                     (.digest (Files/readAllBytes
                                (.toPath (io/file (io/resource "public/js/bootstrap.min.js")))))
                     (->> (.formatHex (HexFormat/of)))
                     (subs 0 12))]
    (is (->> (:body res)
             hick/parse
             hick/as-hickory
             (hsel/select (hsel/attr :src #(= (str "/js/bootstrap.min.js?v=" expected) %)))
             seq)
        "A script's URL includes a hash of its content")))

(deftest an-unknown-url-returns-a-404
  (testing "that is not an api request"
    (let [res (app (req/request :get "/not-a-valid-path"))]
      (is (http-success? res))
      (is (->> (:body res)
               hick/parse
               hick/as-hickory
               (hsel/select (hsel/id "app")))
          "It includes the SPA mount point")))
  (testing "that is not api request"
    (is (http-not-found? (app (req/request :get "/api/not-a-valid-path"))))))

(deftest images-are-returned
  (is (http-success? (app (req/request :get "/images/logo.svg")))))
