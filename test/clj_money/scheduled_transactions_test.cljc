(ns clj-money.scheduled-transactions-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.scheduled-transactions :as st]))

(deftest get-the-next-yearly-transaction-dates
  (let [sched-tran {:interval-type :year
                    :interval-count 1
                    :last-occurrence (t/local-date 2020 3 2)
                    :start-date (t/local-date 2020 3 2)
                    :date-spec {:month 3
                                :day 2}}
        expected [(t/local-date 2021 3 2)]]
    (testing "before the start date"
      (is (empty? (with-fixed-time "2019-03-01T00:00:00Z"
                    (st/next-transaction-dates (dissoc sched-tran :last-occurrence))))))
    (testing "after the start date"
      (testing "before the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-01T00:00:00Z"
                 (st/next-transaction-dates sched-tran)))))
      (testing "on the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-02T00:00:00Z"
                 (st/next-transaction-dates sched-tran)))))
      (testing "after the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-03T00:00:00Z"
                 (st/next-transaction-dates sched-tran))))))
    (testing "start date does not match the spec"
      (is (= expected
             (with-fixed-time "2021-03-03T00:00:00Z"
               (st/next-transaction-dates (assoc sched-tran
                                                 :start-date
                                                 (t/local-date 2020 1 1)))))))))

(deftest get-the-next-monthly-transaction-dates
  (let [sched-tran {:interval-type :month
                    :interval-count 1
                    :last-occurrence (t/local-date 2021 2 2)
                    :start-date (t/local-date 2020 3 2)
                    :date-spec {:day 2}}
        expected [(t/local-date 2021 3 2)]]
    (testing "start date does not match the spec"
      (is (= expected
             (with-fixed-time "2021-03-03T00:00:00Z"
               (st/next-transaction-dates (assoc sched-tran :start-date (t/local-date 2020 3 1)))))))
    (testing "explicit day of the month"
      (testing "before the start date"
        (is (empty? (with-fixed-time "2019-03-01T00:00:00Z"
                      (st/next-transaction-dates (dissoc sched-tran :last-occurrence))))))
      (testing "before the last-occurence"
        (is (empty? (with-fixed-time "2021-02-01T00:00:00Z"
                      (st/next-transaction-dates sched-tran)))))
      (testing "after the start date"
        (testing "before the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-01T00:00:00Z"
                   (st/next-transaction-dates sched-tran)))))
        (testing "on the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-02T00:00:00Z"
                   (st/next-transaction-dates sched-tran)))))
        (testing "after the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-03T00:00:00Z"
                   (st/next-transaction-dates sched-tran)))))))
    (testing ":last day of the month"
      (let [sched-tran (-> sched-tran
                           (assoc :start-date (t/local-date 2020 3 31)
                                  :last-occurrence (t/local-date 2021 2 28))
                           (assoc-in [:date-spec :day] :last))
            expected [(t/local-date 2021 3 31)]]
        (testing "before the start date"
          (is (empty? (with-fixed-time "2019-03-23T00:00:00Z"
                        (st/next-transaction-dates
                          (dissoc sched-tran :last-occurrence))))))
        (testing "after the start date"
          (testing "before the transaction date"
            (is (= expected
                   (with-fixed-time "2021-03-24T00:00:00Z"
                     (st/next-transaction-dates sched-tran)))))
          (testing "on the transaction date"
            (is (= expected
                   (with-fixed-time "2021-03-31T00:00:00Z"
                     (st/next-transaction-dates sched-tran)))))
          (testing "after the transaction date"
            (is (= expected
                   (with-fixed-time "2021-04-06T00:00:00Z"
                     (st/next-transaction-dates sched-tran))))))))
    (testing "irregular last day of month"
      (let [sched-tran (-> sched-tran
                           (assoc :start-date (t/local-date 2019 2 28)
                                  :last-occurrence (t/local-date 2020 1 31))
                           (assoc-in [:date-spec :day] :last))
            expected [(t/local-date 2020 2 29)]]
        (testing "before the start date"
          (is (empty? (with-fixed-time "2018-03-23T00:00:00Z"
                        (st/next-transaction-dates
                          (dissoc sched-tran :last-occurrence))))))
        (testing "after the start date"
          (testing "before the transaction date"
            (is (= expected
                   (with-fixed-time "2020-02-22T00:00:00Z"
                     (st/next-transaction-dates sched-tran)))))
          (testing "on the transaction date"
            (is (= expected
                   (with-fixed-time "2020-02-29T00:00:00Z"
                     (st/next-transaction-dates sched-tran)))))
          (testing "after the transaction date"
            (is (= expected
                   (with-fixed-time "2020-03-06T00:00:00Z"
                     (st/next-transaction-dates sched-tran))))))))))

(deftest get-the-next-weekly-transaction-dates
  (let [sched-tran {:interval-type :week
                    :interval-count 2
                    :last-occurrence (t/local-date 2021 2 24)
                    :start-date (t/local-date 2020 3 2)
                    :date-spec {:days #{:monday}}}
        expected [(t/local-date 2021 3 1)]]
    (testing "start date does not match the spec"
      (is (= expected
             (with-fixed-time "2021-03-03T00:00:00Z"
               (st/next-transaction-dates (assoc sched-tran :start-date (t/local-date 2020 3 1)))))))
    (testing "before the start date"
      (is (empty? (with-fixed-time "2019-03-01T00:00:00Z"
                    (st/next-transaction-dates (dissoc sched-tran :last-occurrence))))))
    (testing "after the start date"
      (testing "before the transaction date"
        (is (= expected
               (with-fixed-time "2021-02-25T00:00:00Z"
                 (st/next-transaction-dates sched-tran)))))
      (testing "on the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-01T00:00:00Z"
                 (st/next-transaction-dates sched-tran)))))
      (testing "after the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-07T00:00:00Z"
                 (st/next-transaction-dates sched-tran))))))
    (testing "close proximity"
      (let [result (with-fixed-time "2021-03-05T00:00:00Z"
                     (st/next-transaction-dates (update-in sched-tran
                                                           [:date-spec :days]
                                                           conj
                                                           :wednesday)))]
        (is (= 2 (count result)) "Two dates are returned")
        (is (t/= (t/local-date 2021 3 1) (first result)) "The first date is correct.")
        (is (t/= (t/local-date 2021 3 3) (second result)) "The second date is correct.")))))

(deftest get-next-transaction-date-with-non-adjacent-periods
  (is (= (t/local-date 2020 9 2)
         (with-fixed-time "2020-03-03T00:00:00Z"
           (st/next-transaction-date {:interval-type :month
                                      :interval-count 6
                                      :date-spec {:day 2}
                                      :last-occurrence (t/local-date 2020 3 2)
                                      :start-date (t/local-date 2019 9 2)})))))
