(ns clj-money.x-platform.util-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-money.test-helpers :refer [pprint-diff]]
            [clj-money.x-platform.util :refer :all]))

(deftest create-a-query-string-from-a-map
  (testing "a single-level map"
    (is (= "first-name=John&last-name=Doe"
           (map->query-string {:first-name "John"
                               :last-name "Doe"}))))
  (testing "a map with map values"
    (is (= "user[first-name]=John&user[last-name]=Doe"
           (map->query-string {:user {:first-name "John"
                                      :last-name "Doe"}}))))
  (testing "a map with vector values"
    (is false "need to write the test"))
  (testing "a deep nested map"
    (is false "need to write the test")))

(deftest create-a-map-from-a-query-string
  (testing "a single-level map"
    (let [expected {:first-name "John"
                    :last-name "Doe"}
          actual (query-string->map "first-name=John&last-name=Doe")]
      (pprint-diff expected actual)
      (is (= expected actual))))
  (testing "a map with map values"
    (let [expected {:user {:first-name "John"
                           :last-name "Doe"}}
          actual (query-string->map "user[first-name]=John&user[last-name]=Doe")]
      (pprint-diff expected actual)
      (is (= expected actual))))
  (testing "a map with vector values"
    (is false "need to write the test"))
  (testing "a deep nested map"
    (is false "need to write the test")))
