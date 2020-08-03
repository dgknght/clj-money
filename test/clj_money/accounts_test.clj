(ns clj-money.accounts-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-time.core :as t]
            [clj-money.accounts :as accounts]))

(deftest create-criteria-from-one-account
  (let [earliest (t/local-date 2020 1 1)
        latest (t/local-date 2020 12 31)
        account {:id 101
                 :earliest-transaction-date earliest
                 :latest-transaction-date latest}]
    (testing "default target field"
      (is (= {:transaction-date [:between earliest latest]
              :account-id 101}
             (accounts/->criteria account))))
    (testing "specified date field"
      (is (= {:end-of-period [:between earliest latest]
              :account-id 101}
             (accounts/->criteria account {:date-field :end-of-period}))))))

(deftest create-criteria-from-multiple-accounts
  (let [earliest (t/local-date 2020 1 1)
        latest (t/local-date 2020 12 31)
        accounts [{:id 101
                   :earliest-transaction-date (t/local-date 2020 2 1)
                   :latest-transaction-date latest}
                  {:id 102
                   :earliest-transaction-date earliest
                   :latest-transaction-date (t/local-date 2020 11 30)}]]
    (is (= {:transaction-date [:between earliest latest]
              :account-id #{101 102}}
             (accounts/->criteria accounts)))))
