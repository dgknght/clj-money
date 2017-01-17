(ns clj-money.pagination-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-money.url :as url]
            [clj-money.pagination :as pagination]))

(deftest create-nav
  ; this test contains an extra level of nesting and isn't necessary to render
  ; the content correctly, but it necessary to acknowledge a side effect
  ; of the logic used to create the result. Probably a better test
  ; should be conceived.
  (let [expected [:nav {:aria-label "Page navigation"}
                  [:ul.pagination
                   [[:li {:class "active"} ; extra nesting
                    [:a {:href "/somewhere/something?page=0&per-page=10"} "1"]]
                   [:li {:class nil}
                    [:a {:href "/somewhere/something?page=1&per-page=10"} "2"]]]]]
        actual (pagination/nav {:page "0"
                                :per-page "10"
                                :url (url/path "/somewhere" "something")
                                :total "15"})]
    (is (= expected actual))))
