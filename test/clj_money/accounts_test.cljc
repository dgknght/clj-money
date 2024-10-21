(ns clj-money.accounts-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.accounts :as accounts]))

(deftest create-criteria-from-one-account
  (is (= #:transaction-item{:transaction-date [:between
                                               (t/local-date 2020 1 1)
                                               (t/local-date 2020 12 31)]
                            :account {:id 101}}
         (accounts/->criteria
           {:id 101
            :account/earliest-transaction-date (t/local-date 2020 1 1)
            :account/latest-transaction-date (t/local-date 2020 12 31)}))))

(deftest create-criteria-from-multiple-accounts
  (is (= {:transaction-item/transaction-date [:between
                                              (t/local-date 2020 1 1)
                                              (t/local-date 2020 2 29)]
          :transaction-item/account #{{:id 101} {:id 102}}}
         (accounts/->>criteria [{:id 101
                                 :account/earliest-transaction-date (t/local-date 2020 2 1)
                                 :account/latest-transaction-date (t/local-date 2020 2 29)}
                                {:id 102
                                 :account/earliest-transaction-date (t/local-date 2020 1 1)
                                 :account/latest-transaction-date (t/local-date 2020 1 31)}]))))

(deftest derive-an-item
  (testing "from a positive quantity"
    (let [quantity 10M]
      (is (= #:transaction-item{:quantity 10M :action :debit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :asset}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity 10M :action :credit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :liability}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity 10M :action :credit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :equity}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity 10M :action :credit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :income}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity 10M :action :debit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :expense}))
          "The action is :debit")))
  (testing "from a negative quantity"
    (let [quantity -10M]
      (is (= #:transaction-item{:quantity 10M :action :credit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :asset}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity 10M :action :debit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :liability}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity 10M :action :debit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :equity}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity 10M :action :debit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :income}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity 10M :action :credit :account {:id 1}}
             (accounts/->transaction-item quantity {:id 1 :account/type :expense}))
          "The action is :credit"))))

(def ^:private flat-accounts
  [{:id :savings
    :account/type :asset
    :account/name "Savings"
    :account/value 1M}
   {:id :savings-car
    :account/parent {:id :savings}
    :account/type :asset
    :account/name "Car"
    :account/value 1000M}
   {:id :savings-reserve
    :account/parent {:id :savings}
    :account/type :asset
    :account/name "Reserve"
    :account/value  2000M}])

(def ^:private nested-accounts
  [{:type :asset
    :accounts
    [{:id :savings
      :account/type :asset
      :account/name "Savings"
      :account/has-children? true
      :account/children-value 3000M
      :account/total-value 3001M
      :account/value 1M
      :account/children [{:id :savings-car
                          :account/parent {:id :savings}
                          :account/type :asset
                          :account/name "Car"
                          :account/value 1000M
                          :account/total-value 1000M}
                         {:id :savings-reserve
                          :account/parent {:id :savings}
                          :account/type :asset
                          :account/name "Reserve"
                          :account/value  2000M
                          :account/total-value 2000M}]}]}
   {:type :liability :accounts []}
   {:type :equity :accounts []}
   {:type :income :accounts []}
   {:type :expense :accounts []}])

(deftest nest-accounts
  (is (= nested-accounts (accounts/nest flat-accounts))))

(deftest check-for-a-user-tag
  (let [account {:account/user-tags #{:mandatory}}]
    (is (accounts/user-tagged? account :mandatory))
    (is (not (accounts/user-tagged? account :discretionary)))))

(deftest create-rebalancing-adjustments
  (let [accounts (->> [{:id :ira
                        :account/name "IRA"
                        :account/quantity 468931.04M
                        :account/total-value 468931.04M
                        :account/value 778.29M
                        :account/allocations {:gold 7.5M
                                              :stocks 30M
                                              :commodities 7.5M
                                              :int-term-bonds 15M
                                              :long-term-bonds 40M}}
                       {:id :gold
                        :account/name "Gold"
                        :account/value 34309.80M}
                       {:id :stocks
                        :account/name "Stocks"
                        :account/value 139853.78M}
                       {:id :commodities
                        :account/name "Commodities"
                        :account/value 38316.93M}
                       {:id :int-term-bonds
                        :account/name "Intermediate Term Bods"
                        :account/value 69167.10M}
                       {:id :long-term-bonds
                        :account/name "Long Term Bods"
                        :account/value 186505.13M}]
                      (map (juxt :id identity))
                      (into {}))
        adjustments (accounts/allocate (accounts :ira)
                                            accounts)]
    (is (= [{:account (accounts :commodities)
             :target-percentage 7.5M
             :target-value 35111.45625M
             :current-percentage 0.0818M
             :current-value 38316.93M
             :adj-value -3200M}
            {:account (accounts :gold)
             :target-percentage 7.5M
             :target-value 35111.45625M
             :current-percentage 0.0733M
             :current-value 34309.80M
             :adj-value 700M}
            {:account (accounts :int-term-bonds)
             :target-percentage 15M
             :target-value 70222.9125M
             :current-percentage 0.148M
             :current-value 69167.10M
             :adj-value 1100M}
            {:account (accounts :long-term-bonds)
             :target-percentage 40M
             :target-value 187261.10M
             :current-percentage 0.398M
             :current-value 186505.13M
             :adj-value 800M}
            {:account (accounts :stocks)
             :target-percentage 30M
             :target-value 140445.825M
             :current-percentage 0.299M
             :current-value 139853.78M
             :adj-value 600M}]
           (sort-by (comp :id :account) adjustments)))
    (is (zero? (->> adjustments
                    (map :adj-value)
                    (reduce +)))
        "The net change is zero")))

(deftest reallocate-for-withdrawal
  (let [accounts (->> [{:id :ira
                        :account/name "IRA"
                        :account/quantity 468931.04M
                        :account/total-value 468931.04M
                        :account/value 778.29M
                        :account/allocations {:gold 7.5M
                                              :stocks 30M
                                              :commodities 7.5M
                                              :int-term-bonds 15M
                                              :long-term-bonds 40M}}
                       {:id :gold
                        :account/name "Gold"
                        :account/value 34309.80M}
                       {:id :stocks
                        :account/name "Stocks"
                        :account/value 139853.78M}
                       {:id :commodities
                        :account/name "Commodities"
                        :account/value 38316.93M}
                       {:id :int-term-bonds
                        :account/name "Intermediate Term Bods"
                        :account/value 69167.10M}
                       {:id :long-term-bonds
                        :account/name "Long Term Bods"
                        :account/value 186505.13M}]
                      (map (juxt :id identity))
                      (into {}))
        withdrawal 10000M
        adjustments (accounts/allocate (accounts :ira) accounts :withdrawal withdrawal)]
    (is (= [{:account (accounts :commodities)
             :target-percentage 7.5M
             :target-value 34361.45625M
             :current-percentage 0.0836M
             :current-value 38316.93M
             :adj-value -4000M}
            {:account (accounts :gold)
             :target-percentage 7.5M
             :target-value 34361.45625M
             :current-percentage 0.0749M ; TODO: multiply this by 100 also?
             :current-value 34309.80M
             :adj-value 0M}
            {:account (accounts :int-term-bonds)
             :target-percentage 15M
             :target-value 68722.9125M
             :current-percentage 0.151M
             :current-value 69167.10M
             :adj-value -400M}
            {:account (accounts :long-term-bonds)
             :target-percentage 40M
             :target-value 183261.100M
             :current-percentage 0.407M
             :current-value 186505.13M
             :adj-value -3200M}
            {:account (accounts :stocks)
             :target-percentage 30M
             :target-value 137445.825M
             :current-percentage 0.305M
             :current-value 139853.78M
             :adj-value -2400M}]
           (sort-by (comp :id :account) adjustments)))
    (is (= (- 0 withdrawal)
           (->> adjustments
                (map :adj-value)
                (reduce +)))
        "The net change is zero")))

(deftest find-account-by-path
  (let [accounts (map #(hash-map :account/path %)
                      [["Investments"]
                       ["Investments" "IRA"]
                       ["Investments" "Eli's College"]
                       ["Investments" "Fidelity"]
                       ["Investments" "Fidelity"]
                       ["Receivable" "Eli"]])]

    (is (= [["Investments" "Eli's College"]
            ["Investments" "Fidelity"]
            ["Investments" "Fidelity"]
            ["Receivable" "Eli"]]
           (map :account/path (accounts/find-by-path "Eli" accounts))))
    (is (= [["Receivable" "Eli"]]
           (map :account/path (accounts/find-by-path "Rec/Eli" accounts))))
    (is (= [["Receivable" "Eli"]]
           (map :account/path (accounts/find-by-path "Rec:Eli" accounts))))))

(defn- test-polarization
  [account-type action quantity expected message]
  (let [account {:account/type account-type}
        item #:transaction-item{:account account
                                :action action
                                :quantity quantity}]
    (is (= expected (accounts/polarize-quantity item account)) message)))

(deftest polarize-a-quantity
  ; Debits
  (test-polarization :asset     :debit 100M  100M "A debit in an asset account increases the balance")
  (test-polarization :expense   :debit 100M  100M "A debit in an expense account increases the balance")
  (test-polarization :liability :debit 100M -100M "A debit in an liability account decreases the balance")
  (test-polarization :equity    :debit 100M -100M "A debit in an equity account decreases the balance")
  (test-polarization :income    :debit 100M -100M "A debit in an income account decreases the balance")

  ;; Credits
  (test-polarization :asset     :credit 100M -100M "A credit in an asset account decreases the balance")
  (test-polarization :expense   :credit 100M -100M "A credit in an expense account dereases the balance")
  (test-polarization :liability :credit 100M  100M "A credit in an liability account increases the balance")
  (test-polarization :equity    :credit 100M  100M "A credit in an equity account increases the balance")
  (test-polarization :income    :credit 100M  100M "A credit in an income account increases the balance"))

(deftest derive-action-from-quantity-and-account
  (is (= :debit  (accounts/derive-action  1 {:account/type :asset})))
  (is (= :credit (accounts/derive-action -1 {:account/type :asset})))
  (is (= :debit  (accounts/derive-action  1 {:account/type :expense})))
  (is (= :credit (accounts/derive-action -1 {:account/type :expense})))
  (is (= :credit (accounts/derive-action  1 {:account/type :income})))
  (is (= :debit  (accounts/derive-action -1 {:account/type :income})))
  (is (= :credit (accounts/derive-action  1 {:account/type :equity})))
  (is (= :debit  (accounts/derive-action -1 {:account/type :equity})))
  (is (= :credit (accounts/derive-action  1 {:account/type :liability})))
  (is (= :debit  (accounts/derive-action -1 {:account/type :liability}))))
