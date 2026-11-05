(ns clj-money.views.transactions.form-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [clj-money.state :as state]
            [clj-money.views.transactions.form :as tf]))

(def ^:private checking
  {:id 1
   :account/name "Checking"
   :account/type :asset})

(def ^:private groceries
  {:id 2
   :account/name "Groceries"
   :account/type :expense})

(use-fixtures :each
  {:before #(reset! state/accounts [checking groceries])
   :after #(reset! state/accounts nil)})

(def ^:private unilateral
  #:transaction{:description "Kroger"
                :items [#:transaction-item{:action :credit
                                           :account {:id 1}
                                           :quantity 10M}
                        #:transaction-item{:action :debit
                                           :account {:id 2}
                                           :quantity 10M}]})

(def ^:private bilateral
  #:transaction{:description "Kroger"
                :items [#:transaction-item{:credit-item #:account-item{:account checking
                                                                       :quantity -10M}
                                           :debit-item #:account-item{:account groceries
                                                                      :quantity 10M}}]})

(deftest prepare-a-transaction-for-edit
  (testing "a transaction that can't be simplified"
    (let [prepared (tf/prepare-for-edit unilateral checking)]
      (is (= [checking groceries {}]
             (map #(:transaction-item/account % {})
                  (:transaction/items prepared)))
          "The full accounts are supplied, and an empty item is added for entry")
      (is (= [10M nil {}]
             (map #(:transaction-item/credit-quantity % {})
                  (:transaction/items prepared)))
          "The quantities are split into credit and debit quantities")))
  (testing "a transaction that can be simplified"
    (let [prepared (tf/prepare-for-edit bilateral checking)]
      (is (= checking (:transaction/account prepared))
          "It is shown from the perspective of the given account")
      (is (= groceries (:transaction/other-account prepared))
          "The other account is the counterpart")))
  (testing "a transaction that can be simplified, without an account"
    (is (nil? (:transaction/account (tf/prepare-for-edit bilateral nil)))
        "It is not simplified")))
