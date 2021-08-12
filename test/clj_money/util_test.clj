(ns clj-money.util-test
  (:require [clojure.test :refer [deftest testing is are]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [clj-time.core :as t]
            [clj-money.util :as util]))

(deftest get-a-descending-list-of-time-periods
  (testing "an infinite list"
    (is (= ["2018-03-01"
            "2018-02-01"
            "2018-01-01"
            "2017-12-01"]
           (->> (util/desc-periodic-seq (t/local-date 2018 3 1) (t/months 1))
                (map serialize-date)
                (take 4)))))
  (testing "a specified range"
    (is (= ["2018-03-02"
            "2018-01-02"
            "2017-11-02"]
           (->> (util/desc-periodic-seq (t/local-date 2017 11 1)
                                        (t/local-date 2018 3 2)
                                        (t/months 2))
                (map serialize-date))))))

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
    (are [m k expected] (= expected (util/nominal-comparatives m k))
         {:start-on date}
         :start
         {:start-on date}

         {:transaction-date date}
         :transaction-date
         {:transaction-date date}

         {:start-on [:> date]}
         :start
         {:start-after date}

         {:transaction-date [:> date]}
         :transaction-date
         {:transaction-date-after date}

         {:transaction-date [:between date other-date]}
         :transaction-date
         {:transaction-date-on-or-after date
          :transaction-date-on-or-before other-date}

         {:start-on [:between date other-date]}
         :start-on
         {:start-on-or-after date
          :start-on-or-before other-date})))

(deftest get-the-earliest-date
  (let [d1 (t/local-date 2020 3 2)
        d2 (t/local-date 2020 2 27)]
    (is (= d2 (util/earliest d1 d2)))))
