(ns clj-money.scheduled-transactions-test
  (:require [clojure.test :refer [deftest is testing]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.scheduled-transactions :as st]))

(deftest get-the-next-yearly-transaction-dates
  (let [sched-trx #:scheduled-transaction{:period [1 :year]
                                          :last-occurrence (t/local-date 2020 3 2)
                                          :start-date (t/local-date 2020 3 2)
                                          :date-spec {:month 3
                                                      :day 2}}
        expected [(t/local-date 2021 3 2)]]
    (testing "before the start date"
      (is (empty? (with-fixed-time "2019-03-01T00:00:00Z"
                    (st/next-transaction-dates
                      (dissoc sched-trx :scheduled-transaction/last-occurrence))))))
    (testing "after the start date"
      (testing "before the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-01T00:00:00Z"
                 (st/next-transaction-dates sched-trx)))))
      (testing "on the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-02T00:00:00Z"
                 (st/next-transaction-dates sched-trx)))))
      (testing "after the transaction date"
        (is (= expected
               (with-fixed-time "2021-03-03T00:00:00Z"
                 (st/next-transaction-dates sched-trx))))))
    (testing "start date does not match the spec"
      (is (= expected
             (with-fixed-time "2021-03-03T00:00:00Z"
               (st/next-transaction-dates (assoc sched-trx
                                                 :scheduled-transaction/start-date
                                                 (t/local-date 2020 1 1)))))))))

(deftest get-the-next-monthly-transaction-dates
  (let [sched-trx #:scheduled-transaction{:period [1 :month]
                                          :last-occurrence (t/local-date 2021 2 2)
                                          :start-date (t/local-date 2020 3 2)
                                          :date-spec {:day 2}}
        expected [(t/local-date 2021 3 2)]]
    (testing "start date does not match the spec"
      (is (= expected
             (with-fixed-time "2021-03-03T00:00:00Z"
               (st/next-transaction-dates
                 (assoc sched-trx
                        :scheduled-transaction/start-date
                        (t/local-date 2020 3 1)))))))
    (testing "explicit day of the month"
      (testing "before the start date"
        (is (empty? (with-fixed-time "2019-03-01T00:00:00Z"
                      (st/next-transaction-dates
                        (dissoc sched-trx :scheduled-transaction/last-occurrence))))))
      (testing "before the last-occurence"
        (is (empty? (with-fixed-time "2021-02-01T00:00:00Z"
                      (st/next-transaction-dates sched-trx)))))
      (testing "after the start date"
        (testing "before the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-01T00:00:00Z"
                   (st/next-transaction-dates sched-trx)))))
        (testing "on the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-02T00:00:00Z"
                   (st/next-transaction-dates sched-trx)))))
        (testing "after the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-03T00:00:00Z"
                   (st/next-transaction-dates sched-trx)))))))
    (testing ":last day of the month"
      (let [sched-trx (-> sched-trx
                          (assoc :scheduled-transaction/start-date (t/local-date 2020 3 31)
                                 :scheduled-transaction/last-occurrence (t/local-date 2021 2 28))
                          (assoc-in [:scheduled-transaction/date-spec :day] :last))
            expected [(t/local-date 2021 3 31)]]
        (testing "before the start date"
          (is (empty? (with-fixed-time "2019-03-23T00:00:00Z"
                        (st/next-transaction-dates
                          (dissoc sched-trx :scheduled-transaction/last-occurrence))))))
        (testing "after the start date"
          (testing "before the transaction date"
            (is (= expected
                   (with-fixed-time "2021-03-24T00:00:00Z"
                     (st/next-transaction-dates sched-trx)))))
          (testing "on the transaction date"
            (is (= expected
                   (with-fixed-time "2021-03-31T00:00:00Z"
                     (st/next-transaction-dates sched-trx)))))
          (testing "after the transaction date"
            (is (= expected
                   (with-fixed-time "2021-04-06T00:00:00Z"
                     (st/next-transaction-dates sched-trx))))))))
    (testing "irregular last day of month"
      (let [sched-tran (-> sched-trx
                           (assoc :scheduled-transaction/start-date (t/local-date 2019 2 28)
                                  :scheduled-transaction/last-occurrence (t/local-date 2020 1 31))
                           (assoc-in [:scheduled-transaction/date-spec :day] :last))
            expected [(t/local-date 2020 2 29)]]
        (testing "before the start date"
          (is (empty? (with-fixed-time "2018-03-23T00:00:00Z"
                        (st/next-transaction-dates
                          (dissoc sched-tran :scheduled-transaction/last-occurrence))))))
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
  (let [sched-trx #:scheduled-transaction{:period [2 :week]
                                          :last-occurrence (t/local-date 2021 2 24)
                                          :start-date (t/local-date 2020 3 2)
                                          :date-spec {:days #{:monday}}}
        expected [(t/local-date 2021 3 1)]]
    (testing "start date does not match the spec"
      (is (= expected
             (with-fixed-time "2021-03-03T00:00:00Z"
               (st/next-transaction-dates
                 (assoc sched-trx
                        :scheduled-transaction/start-date
                        (t/local-date 2020 3 1)))))
          "It returns the next date in the sequence"))
    (is (empty? (with-fixed-time "2019-03-01T00:00:00Z"
                    (st/next-transaction-dates
                      (dissoc sched-trx :scheduled-transaction/last-occurrence))))
          "Return an empty sequence when the specified start is before the scheduled start")
    (testing "after the start date"
        (testing "before the transaction date"
          (is (= expected
                 (with-fixed-time "2021-02-25T00:00:00Z"
                   (st/next-transaction-dates sched-trx)))
              "It returns the next date in the sequence after the specified start"))
        (testing "on the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-01T00:00:00Z"
                   (st/next-transaction-dates sched-trx)))
              "It returns the given date"))
        (testing "after the transaction date"
          (is (= expected
                 (with-fixed-time "2021-03-07T00:00:00Z"
                   (st/next-transaction-dates sched-trx)))
              "It returns the first date in the sequence after the given date")))
    (testing "close proximity"
      (let [result (with-fixed-time "2021-03-05T00:00:00Z"
                     (st/next-transaction-dates
                       (update-in sched-trx
                                  [:scheduled-transaction/date-spec :days]
                                  conj
                                  :wednesday)))]
        (is (= 2 (count result)) "Two dates are returned")
        (is (t/= (t/local-date 2021 3 1) (first result))
            "The first date is the next in the sequence")
        (is (t/= (t/local-date 2021 3 3) (second result))
            "The second date is the following date in the sequence")))))

(deftest get-next-transaction-date-with-non-adjacent-periods
  (is (= (t/local-date 2020 9 2)
         (with-fixed-time "2020-03-03T00:00:00Z"
           (st/next-transaction-date
             #:scheduled-transaction{:period [6 :month]
                                     :date-spec {:day 2}
                                     :last-occurrence (t/local-date 2020 3 2)
                                     :start-date (t/local-date 2019 9 2)})))))

(deftest realize-a-scheduled-transaction-after-the-date
  (with-fixed-time "2016-02-02T00:00:00Z"
    (let [expected [#:scheduled-transaction{:last-occurrence (t/local-date 2016 2 1)}
                    #:transaction{:description "Paycheck"
                                  :transaction-date (t/local-date 2016 1 1)
                                  :scheduled-transaction {:id 101}
                                  :items [#:transaction-item{:action :debit
                                                             :account {:id :checking}
                                                             :quantity 900M}
                                          #:transaction-item{:action :debit
                                                             :account {:id :fit}
                                                             :quantity 100M}
                                          #:transaction-item{:action :credit
                                                             :account {:id :salary}
                                                             :quantity 1000M}]}
                    #:transaction{:description "Paycheck"
                                  :transaction-date (t/local-date 2016 2 1)
                                  :scheduled-transaction {:id 101}
                                  :items [#:transaction-item{:action :debit
                                                             :account {:id :checking}
                                                             :quantity 900M}
                                          #:transaction-item{:action :debit
                                                             :account {:id :fit}
                                                             :quantity 100M}
                                          #:transaction-item{:action :credit
                                                             :account {:id :salary}
                                                             :quantity 1000M}]}]
          actual (st/realize
                   {:id 101
                    :scheduled-transaction/entity "Personal"
                    :scheduled-transaction/description "Paycheck"
                    :scheduled-transaction/start-date (t/local-date 2016 1 1)
                    :scheduled-transaction/date-spec {:day 1}
                    :scheduled-transaction/period [1 :month]
                    :scheduled-transaction/items [#:scheduled-transaction-item{:action :debit
                                                                               :account {:id :checking}
                                                                               :quantity 900M}
                                                  #:scheduled-transaction-item{:action :debit
                                                                               :account {:id :fit}
                                                                               :quantity 100M}
                                                  #:scheduled-transaction-item{:action :credit
                                                                               :account {:id :salary}
                                                                               :quantity 1000M}]})]
      (is (seq-of-maps-like? expected actual)))))
