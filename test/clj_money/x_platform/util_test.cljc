(ns clj-money.x-platform.util-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.format :as f]
               :cljs [cljs-time.format :as f])
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
    (is (= "names[]=john&names[]=jane"
           (map->query-string {:names ["john" "jane"]}))))
  (testing "a deep nested map"
    (let [expected "criteria[transaction-date][]=between&criteria[transaction-date][]=2018-02-27&criteria[transaction-date][]=2018-03-02&options[sort][]=transaction-date&options[sort][]=desc&options[limit]=50"
          actual (map->query-string {:criteria {:transaction-date [:between (t/local-date 2018 2 27) (t/local-date 2018 3 2)]}
                                     :options {:sort [:transaction-date :desc]
                                               :limit 50}})]
      (is (= expected actual)))))

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
    (is (= {:names ["john" "jane"]}
           (query-string->map "names[]=john&names[]=jane"))))
  (testing "a deep nested map"
    (let [expected {:criteria {:transaction-date ["between" "2018-02-27" "2018-03-02"]}
                    :options {:sort ["transaction-date" "desc"]
                              :limit "50"}}
          actual (query-string->map "criteria[transaction-date][]=between&criteria[transaction-date][]=2018-02-27&criteria[transaction-date][]=2018-03-02&options[sort][]=transaction-date&options[sort][]=desc&options[limit]=50")]
      (pprint-diff expected actual)
      (is (= expected actual)))))

(deftest get-a-descending-list-of-time-periods
  (testing "an infinite list"
    (is (= ["2018-03-01"
            "2018-02-01"
            "2018-01-01"
            "2017-12-01"]
           (->> (desc-periodic-seq (t/local-date 2018 3 1) (t/months 1))
                (map #(f/unparse-local (:date f/formatters) %))
                (take 4)))))
  (testing "a specified range"
    (is (= ["2018-03-02"
            "2018-01-02"
            "2017-11-02"]
           (->> (desc-periodic-seq (t/local-date 2017 11 1)
                                   (t/local-date 2018 3 2)
                                   (t/months 2))
                (map #(f/unparse-local (:date f/formatters) %)))))))
