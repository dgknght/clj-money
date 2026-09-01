(ns clj-money.reconciliations-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is]])
            [clj-money.decimal :as d]
            [clj-money.reconciliations :as reconciliations]))

(deftest a-liability-account-requires-payment
  (is (reconciliations/requires-payment? #:account{:type :liability})))

(deftest an-asset-account-does-not-require-payment
  (is (not (reconciliations/requires-payment? #:account{:type :asset}))))

(deftest build-a-payment-template-for-a-reconciled-account
  (let [account #:account{:id :credit-card
                          :name "Credit Card"
                          :type :liability}]
    (is (= #:transaction{:other-account account
                         :quantity (d/d 100)}
           (reconciliations/->payment account (d/d 100))))))
