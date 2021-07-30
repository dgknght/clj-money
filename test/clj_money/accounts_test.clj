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

(deftest derive-an-item
  (testing "from a positive quantity"
    (let [quantity 10M]
      (is (= {:quantity 10M :action :debit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :asset}))
          "The action is :debit")
      (is (= {:quantity 10M :action :credit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :liability}))
          "The action is :credit")
      (is (= {:quantity 10M :action :credit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :equity}))
          "The action is :credit")
      (is (= {:quantity 10M :action :credit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :income}))
          "The action is :credit")
      (is (= {:quantity 10M :action :debit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :expense}))
          "The action is :debit")))
  (testing "from a negative quantity"
    (let [quantity -10M]
      (is (= {:quantity 10M :action :credit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :asset}))
          "The action is :credit")
      (is (= {:quantity 10M :action :debit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :liability}))
          "The action is :debit")
      (is (= {:quantity 10M :action :debit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :equity}))
          "The action is :debit")
      (is (= {:quantity 10M :action :debit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :income}))
          "The action is :debit")
      (is (= {:quantity 10M :action :credit :account-id 1}
             (accounts/derive-item quantity {:id 1 :type :expense}))
          "The action is :credit"))))

(def ^:private flat-accounts
  [{:id 1
    :type :asset
    :name "Savings"
    :value 1M}
   {:id 2
    :parent-id 1
    :type :asset
    :name "Car"
    :value 1000M}
   {:id 2
    :parent-id 1
    :type :asset
    :name "Reserve"
    :value  2000M}])

(def ^:private nested-accounts
  [{:type :asset
    :accounts
    [{:id 1
      :type :asset
      :name "Savings"
      :has-children? true
      :children-value 3000M
      :total-value 3001M
      :value 1M
      :children [{:id 2
                  :parent-id 1
                  :type :asset
                  :name "Car"
                  :value 1000M
                  :total-value 1000M}
                 {:id 2
                  :parent-id 1
                  :type :asset
                  :name "Reserve"
                  :value  2000M
                  :total-value 2000M}]}]}
   {:type :liability :accounts []}
   {:type :equity :accounts []}
   {:type :income :accounts []}
   {:type :expense :accounts []}])

(deftest nest-accounts
  (is (= nested-accounts (accounts/nest flat-accounts))))

(deftest check-for-a-user-tag
  (let [account {:user-tags #{:mandatory}}]
    (is (accounts/user-tagged? account :mandatory))
    (is (not (accounts/user-tagged? account :discretionary)))))
