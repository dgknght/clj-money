(ns clj-money.util-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-time.core :as t]
            [clj-money.test-helpers :refer [pprint-diff]]
            [clj-money.util :as util])
  (:import java.util.UUID))

(deftest format-a-decimal
  (is (= "1,234.50" (util/format-decimal 1234.5M))
      "The default format uses a comma, no currency symbol, and 2 decimal places"))

(deftest format-a-percent
  (is (= "25.4%" (util/format-percent 0.254M))))

(deftest get-a-uuid
  (let [str-id "10000000-0000-0000-0000-000000000000"
        id (UUID/fromString str-id)]
    (is (= id (util/uuid str-id))
        "A string is parsed into a uuid")
    (is (= id (util/uuid id))
        "A UUID is returned as-is")
    (is (instance? UUID (util/uuid))
        "With no args, a new UUID is returned")))
(deftest create-a-query-string-from-a-map
  (testing "a single-level map"
    (is (= "first-name=John&last-name=Doe"
           (util/map->query-string {:first-name "John"
                               :last-name "Doe"}))))
  (testing "a map with map values"
    (is (= "user[first-name]=John&user[last-name]=Doe"
           (util/map->query-string {:user {:first-name "John"
                                      :last-name "Doe"}}))))
  (testing "a map with vector values"
    (is (= "names[]=john&names[]=jane"
           (util/map->query-string {:names ["john" "jane"]}))))
  (testing "a deep nested map"
    (let [expected "criteria[transaction-date][]=between&criteria[transaction-date][]=2018-02-27&criteria[transaction-date][]=2018-03-02&options[sort][]=transaction-date&options[sort][]=desc&options[limit]=50"
          actual (util/map->query-string {:criteria {:transaction-date [:between (t/local-date 2018 2 27) (t/local-date 2018 3 2)]}
                                     :options {:sort [:transaction-date :desc]
                                               :limit 50}})]
      (is (= expected actual)))))

(deftest create-a-map-from-a-query-string
  (testing "a single-level map"
    (let [expected {:first-name "John"
                    :last-name "Doe"}
          actual (util/query-string->map "first-name=John&last-name=Doe")]
      (pprint-diff expected actual)
      (is (= expected actual))))
  (testing "a map with map values"
    (let [expected {:user {:first-name "John"
                           :last-name "Doe"}}
          actual (util/query-string->map "user[first-name]=John&user[last-name]=Doe")]
      (pprint-diff expected actual)
      (is (= expected actual))))
  (testing "a map with vector values"
    (is (= {:names ["john" "jane"]}
           (util/query-string->map "names[]=john&names[]=jane"))))
  (testing "a deep nested map"
    (let [expected {:criteria {:transaction-date ["between" "2018-02-27" "2018-03-02"]}
                    :options {:sort ["transaction-date" "desc"]
                              :limit "50"}}
          actual (util/query-string->map "criteria[transaction-date][]=between&criteria[transaction-date][]=2018-02-27&criteria[transaction-date][]=2018-03-02&options[sort][]=transaction-date&options[sort][]=desc&options[limit]=50")]
      (pprint-diff expected actual)
      (is (= expected actual)))))

(deftest get-a-descending-list-of-time-periods
  (testing "an infinite list"
    (is (= ["2018-03-01"
            "2018-02-01"
            "2018-01-01"
            "2017-12-01"]
           (->> (util/desc-periodic-seq (t/local-date 2018 3 1) (t/months 1))
                (map util/serialize-date)
                (take 4)))))
  (testing "a specified range"
    (is (= ["2018-03-02"
            "2018-01-02"
            "2017-11-02"]
           (->> (util/desc-periodic-seq (t/local-date 2017 11 1)
                                        (t/local-date 2018 3 2)
                                        (t/months 2))
                (map util/serialize-date))))))

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

(deftest convert-nominal-comparatives-to-symbolic
  (let [date (t/local-date 2015 1 1)
        other-date (t/local-date 2015 1 31)]
    (is (= {:start-on [:> date]}
           (util/symbolic-comparatives {:start-after date}
                                       :start)))
    (is (= {:transaction-date [:> date]}
           (util/symbolic-comparatives {:transaction-date-after date}
                                       :transaction-date))
        "Keys that end with -date don't receive -on or -at")
    (is (= {:transaction-date [:between date other-date]}
           (util/symbolic-comparatives {:transaction-date-on-or-after date
                                        :transaction-date-on-or-before other-date}
                                       :transaction-date)))
    (is (= {:start-on [:between date other-date]}
           (util/symbolic-comparatives {:start-on-or-after date
                                        :start-on-or-before other-date}
                                       :start-on)))))

(deftest convert-symbolic-comparatives-to-nominal
  (let [date (t/local-date 2015 1 1)
        other-date (t/local-date 2015 1 31)]
    (is (= {:start-after date}
           (util/nominal-comparatives {:start-on [:> date]}
                                      :start)))
    (is (= {:transaction-date-after date}
           (util/nominal-comparatives {:transaction-date [:> date]}
                                      :transaction-date)))
    (is (= {:transaction-date-on-or-after date
            :transaction-date-on-or-before other-date}
           (util/nominal-comparatives {:transaction-date [:between date other-date]}
                                      :transaction-date))
        "between generates to keys in the result")
    (is (= {:start-on-or-after date
            :start-on-or-before other-date}
           (util/nominal-comparatives {:start-on [:between date other-date]}
                                      :start-on))
        "between generates to keys in the result")))
