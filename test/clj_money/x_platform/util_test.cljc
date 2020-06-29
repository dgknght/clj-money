(ns clj-money.x-platform.util-test
  (:require [clojure.test :refer [deftest testing is]]
            #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.format :as f]
               :cljs [cljs-time.format :as f])
            [clj-money.test-helpers :refer [pprint-diff]]
            [clj-money.x-platform.util
             :as util
             :refer [map->query-string
                     query-string->map
                     desc-periodic-seq]]))

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

(deftest build-a-path
  (is (= "/api/users/me"
         (util/path :api :users :me))))

(deftest conditionally-update-in
  (is (= {:value 2}
         (util/update-in-if {:value 1}
                            [:value]
                            inc))
      "The fn is applied if the value is present")
  (is (= {}
         (util/update-in-if {}
                            [:value]
                            inc))
      "The map is not modified if the key is not present"))

(deftest test-for-deeply-contained-key
  (is (util/deep-contains? {:one 1} :one))
  (is (util/deep-contains? [:and {:one 1}] :one)))

(deftest find-deeply-contained-value
  (is (= 1 (util/deep-get {:one 1} :one)))
  (is (= 1 (util/deep-get [:and {:one 1}] :one))))

(deftest dissoc-deeply-contained-value
  (is (= [:or [{:account-id 1}
               {:description "test"}]]
         (util/deep-dissoc [:or [{:account-id 1
                                  :reconciled true}
                                 {:description "test"}]]
                           :reconciled))))

(deftest parse-a-boolean
  (is (util/parse-bool "true"))
  (is (util/parse-bool "True"))
  (is (util/parse-bool "TRUE"))
  (is (util/parse-bool "1"))
  (is (not (util/parse-bool "false")))
  (is (not (util/parse-bool "False")))
  (is (not (util/parse-bool "FALSE")))
  (is (not (util/parse-bool "0"))))
