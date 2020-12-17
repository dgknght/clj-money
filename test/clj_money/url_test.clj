(ns clj-money.url-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.url :as url]))

(deftest create-a-relative-url
  (is (= "somepath/somepage.html"
         (-> (url/path "somepath" "somepage.html")
             url/format-url))
      "The correct URL should be returned"))

(deftest create-a-relative-root-url
  (is (= "/somepath/somepage.html"
         (-> (url/path "/somepath" "somepage.html")
             url/format-url))
      "The correct URL should be returned"))

(deftest create-an-absolute-url
  (is (= "http://somehost.com/somepath/somepage.html"
         (-> (url/host "somehost.com")
             (url/protocol "http")
             (url/path "somepath" "somepage.html")
             url/format-url))
      "The correct URL should be returned"))

(deftest include-query-string-values
  (is (= "http://somehost.com/somepath/somepage.html?min=1&max=10"
         (-> (url/host "somehost.com")
             (url/protocol "http")
             (url/path "somepath" "somepage.html")
             (url/query {:min 1
                         :max 10})
             url/format-url))
      "The correct URL should be returned"))

; TODO discard extraneous slashes within segments
; TODO discard initial slash when a host is specified
