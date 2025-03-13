(ns clj-money.scheduled-transactions-test
  (:require [cljs.test :refer [deftest is testing]]
            [cljs-time.core :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.scheduled-transactions :as st]))

(deftest get-the-next-yearly-transaction-dates
  (let [sched-trx #:scheduled-transaction{:interval-type :year
                                          :interval-count 1
                                          :last-occurrence (t/local-date 2020 3 2)
                                          :start-date (t/local-date 2020 3 2)
                                          :date-spec {:month 3
                                                      :day 2}}
        expected [(t/local-date 2021 3 2)]]
    (testing "before the start date"
      (is (empty? (t/do-at*
                    (t/date-time 2019 3 1)
                    #(st/next-transaction-dates
                       (dissoc sched-trx :scheduled-transaction/last-occurrence))))))
    (testing "after the start date"
      (testing "before the transaction date"
        (is (= expected
               (t/do-at*
                 (t/date-time 2021 3 1)
                 #(st/next-transaction-dates sched-trx)))))
      (testing "on the transaction date"
        (is (= expected
               (t/do-at*
                 (t/date-time 2021 3 2)
                 #(st/next-transaction-dates sched-trx)))))
      (testing "after the transaction date"
        (is (= expected
               (t/do-at*
                 (t/date-time 2021 3 3)
                 #(st/next-transaction-dates sched-trx))))))
    (testing "start date does not match the spec"
      (is (= expected
             (t/do-at*
               (t/date-time 2021 3 3)
               #(st/next-transaction-dates (assoc sched-trx
                                                  :scheduled-transaction/start-date
                                                  (t/local-date 2020 1 1)))))))))

(deftest get-the-next-monthly-transaction-dates
  (let [sched-trx #:scheduled-transaction{:interval-type :month
                                          :interval-count 1
                                          :last-occurrence (t/local-date 2021 2 2)
                                          :start-date (t/local-date 2020 3 2)
                                          :date-spec {:day 2}}
        expected [(t/local-date 2021 3 2)]]
    (testing "start date does not match the spec"
      (is (= expected
             (t/do-at*
               (t/date-time 2021 03 03)
               #(st/next-transaction-dates
                  (assoc sched-trx
                         :scheduled-transaction/start-date
                         (t/local-date 2020 3 1)))))))
    (testing "explicit day of the month"
      (testing "before the start date"
        (is (empty? (t/do-at*
                      (t/date-time 2019 03 01)
                      #(st/next-transaction-dates
                         (dissoc sched-trx :scheduled-transaction/last-occurrence))))))
      (testing "before the last-occurence"
        (is (empty? (t/do-at*
                      (t/date-time 2021 02 01)
                      #(st/next-transaction-dates sched-trx)))))
      (testing "after the start date"
        (testing "before the transaction date"
          (is (= expected
                 (t/do-at*
                   (t/date-time 2021 03 01)
                   #(st/next-transaction-dates sched-trx)))))
        (testing "on the transaction date"
          (is (= expected
                 (t/do-at*
                   (t/date-time 2021 03 02)
                   #(st/next-transaction-dates sched-trx)))))
        (testing "after the transaction date"
          (is (= expected
                 (t/do-at*
                   (t/date-time 2021 03 03)
                   #(st/next-transaction-dates sched-trx)))))))
    (testing ":last day of the month"
      (let [sched-trx (-> sched-trx
                          (assoc :scheduled-transaction/start-date (t/local-date 2020 3 31)
                                 :scheduled-transaction/last-occurrence (t/local-date 2021 2 28))
                          (assoc-in [:scheduled-transaction/date-spec :day] :last))
            expected [(t/local-date 2021 3 31)]]
        (testing "before the start date"
          (is (empty? (t/do-at*
                        (t/date-time 2019 03 23)
                        #(st/next-transaction-dates
                           (dissoc sched-trx :scheduled-transaction/last-occurrence))))
              "No dates are returned because we have not reached the 1st occurrence"))
        (testing "after the start date"
          (testing "before the transaction date"
            (is (= expected
                   (t/do-at*
                     (t/date-time 2021 03 24 12)
                     #(st/next-transaction-dates sched-trx)))
                "One future transaction date is returned"))
          (testing "on the transaction date"
            (is (= expected
                   (t/do-at*
                     (t/date-time 2021 03 31)
                     #(st/next-transaction-dates sched-trx)))))
          (testing "after the transaction date"
            (is (= expected
                   (t/do-at*
                     (t/date-time 2021 04 06)
                     #(st/next-transaction-dates sched-trx))))))))
    (testing "irregular last day of month"
      (let [sched-tran (-> sched-trx
                           (assoc :scheduled-transaction/start-date (t/local-date 2019 2 28)
                                  :scheduled-transaction/last-occurrence (t/local-date 2020 1 31))
                           (assoc-in [:scheduled-transaction/date-spec :day] :last))
            expected [(t/local-date 2020 2 29)]]
        (testing "before the start date"
          (is (empty? (t/do-at*
                        (t/date-time 2018 03 23)
                        #(st/next-transaction-dates
                           (dissoc sched-tran :scheduled-transaction/last-occurrence))))))
        (testing "after the start date"
          (testing "before the transaction date"
            (is (= expected
                   (t/do-at*
                     (t/date-time 2020 02 22 12)
                     #(st/next-transaction-dates sched-tran)))))
          (testing "on the transaction date"
            (is (= expected
                   (t/do-at*
                     (t/date-time 2020 02 29)
                     #(st/next-transaction-dates sched-tran)))))
          (testing "after the transaction date"
            (is (= expected
                   (t/do-at*
                     (t/date-time 2020 03 06)
                     #(st/next-transaction-dates sched-tran))))))))))

(deftest get-the-next-weekly-transaction-dates
  (let [sched-trx #:scheduled-transaction{:interval-type :week
                                          :interval-count 2
                                          :last-occurrence (t/local-date 2021 2 24)
                                          :start-date (t/local-date 2020 3 2)
                                          :date-spec {:days #{:monday}}}
        expected [(t/local-date 2021 3 1)]]
    (testing "start date does not match the spec"
      (is (= expected
             (t/do-at*
               (t/date-time 2021 03 03)
               #(st/next-transaction-dates
                  (assoc sched-trx
                         :scheduled-transaction/start-date
                         (t/local-date 2020 3 1)))))))
    (testing "before the start date"
      (is (empty? (t/do-at*
                    (t/date-time 2019 03 01)
                    #(st/next-transaction-dates
                       (dissoc sched-trx :scheduled-transaction/last-occurrence))))))
    (testing "after the start date"
      (testing "before the transaction date"
        (is (= expected
               (t/do-at*
                 (t/date-time 2021 02 25)
                 #(st/next-transaction-dates sched-trx)))))
      (testing "on the transaction date"
        (is (= expected
               (t/do-at*
                 (t/date-time 2021 03 01)
                 #(st/next-transaction-dates sched-trx)))))
      (testing "after the transaction date"
        (is (= expected
               (t/do-at*
                 (t/date-time 2021 03 07)
                 #(st/next-transaction-dates sched-trx))))))
    (testing "close proximity"
      (let [result (t/do-at*
                     (t/date-time 2021 03 05)
                     #(st/next-transaction-dates
                        (update-in sched-trx
                                   [:scheduled-transaction/date-spec :days]
                                   conj
                                   :wednesday)))]
        (is (= 2 (count result)) "Two dates are returned")
        (is (t/= (t/local-date 2021 3 1) (first result)) "The first date is correct.")
        (is (t/= (t/local-date 2021 3 3) (second result)) "The second date is correct.")))))

(deftest get-next-transaction-date-with-non-adjacent-periods
  (is (= (t/local-date 2020 9 2)
         (t/do-at*
           (t/date-time 2020 03 03)
           #(st/next-transaction-date
              #:scheduled-transaction{:interval-type :month
                                      :interval-count 6
                                      :date-spec {:day 2}
                                      :last-occurrence (t/local-date 2020 3 2)
                                      :start-date (t/local-date 2019 9 2)})))))

(deftest realize-a-scheduled-transaction-after-the-date
  (t/do-at*
    (t/date-time 2016 02 02)
    #(let [expected [#:scheduled-transaction{:last-occurrence (t/local-date 2016 2 1)}
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
                     :scheduled-transaction/interval-type :month
                     :scheduled-transaction/interval-count 1
                     :scheduled-transaction/items [#:scheduled-transaction-item{:action :debit
                                                                                :account {:id :checking}
                                                                                :quantity 900M}
                                                   #:scheduled-transaction-item{:action :debit
                                                                                :account {:id :fit}
                                                                                :quantity 100M}
                                                   #:scheduled-transaction-item{:action :credit
                                                                                :account {:id :salary}
                                                                                :quantity 1000M}]})]
       (is (dgknght.app-lib.test-assertions/seq-of-maps-like?
             expected
             actual)))))
