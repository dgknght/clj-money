(ns clj-money.url-test
  (:require [clojure.test :refer :all]
            [clj-money.url :refer :all]))

(deftest create-a-relative-url
  (is (= "somepath/somepage.html"
         (-> (path "somepath" "somepage.html")
             format-url))
      "The correct URL should be returned"))

(deftest create-a-relative-root-url
  (is (= "/somepath/somepage.html"
         (-> (path "/somepath" "somepage.html")
             format-url))
      "The correct URL should be returned"))

(deftest create-an-absolute-url
  (is (= "http://somehost.com/somepath/somepage.html"
         (-> (host "somehost.com")
             (protocol "http")
             (path "somepath" "somepage.html")
             format-url))
      "The correct URL should be returned"))

(deftest include-query-string-values
  (is (= "http://somehost.com/somepath/somepage.html?min=1&max=10"
         (-> (host "somehost.com")
             (protocol "http")
             (path "somepath" "somepage.html")
             (query {:min 1
                     :max 10})
             format-url))
      "The correct URL should be returned"))

; TODO discard extraneous slashes within segments
; TODO discard initial slash when a host is specified
