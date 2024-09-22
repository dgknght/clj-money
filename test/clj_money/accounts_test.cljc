(ns clj-money.accounts-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [dgknght.app-lib.core :refer [index-by]]
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

(def ^:private allocation-context
  {:ira             {:id 100
                     :name "IRA"
                     :quantity 468931.04M
                     :total-value 468931.04M
                     :value 778.29M
                     :allocations {101 7.5M
                                   102 30M
                                   103 7.5M
                                   104 15M
                                   105 40M}}
   :gold            {:id 101
                     :name "Gold"
                     :value 34309.80M}
   :stocks          {:id 102
                     :name "Stocks"
                     :value 139853.78M}
   :commodities     {:id 103
                     :name "Commodities"
                     :value 38316.93M}
   :int-term-bonds  {:id 104
                     :name "Intermediate Term Bods"
                     :value 69167.10M}
   :long-term-bonds {:id 105
                     :name "Long Term Bods"
                     :value 186505.13M}})

(deftest create-rebalancing-adjustments
  (let [{:keys [ira
                gold
                stocks
                commodities
                int-term-bonds
                long-term-bonds]} allocation-context
        all-accounts (index-by :id [ira
                                    gold
                                    stocks
                                    commodities
                                    int-term-bonds
                                    long-term-bonds])
        adjustments (accounts/allocate ira all-accounts)]
    (is (= [{:account gold
             :target-percentage 7.5M
             :target-value 35111.45625M
             :current-percentage 0.0733M
             :current-value 34309.80M
             :adj-value 800M}
            {:account stocks
             :target-percentage 30M
             :target-value 140445.825M
             :current-percentage 0.299M
             :current-value 139853.78M
             :adj-value 600M}
            {:account commodities
             :target-percentage 7.5M
             :target-value 35111.45625M
             :current-percentage 0.0818M
             :current-value 38316.93M
             :adj-value -3300M}
            {:account int-term-bonds
             :target-percentage 15M
             :target-value 70222.9125M
             :current-percentage 0.148M
             :current-value 69167.10M
             :adj-value 1100M}
            {:account long-term-bonds
             :target-percentage 40M
             :target-value 187261.10M
             :current-percentage 0.398M
             :current-value 186505.13M
             :adj-value 800M}]
           (sort-by (comp :id :account) adjustments)))
    (is (zero? (->> adjustments
                    (map :adj-value)
                    (reduce +)))
        "The net change is zero")))

(deftest reallocate-for-withdrawal
  (let [{:keys [ira
                gold
                stocks
                commodities
                int-term-bonds
                long-term-bonds]} allocation-context
        all-accounts (index-by :id [ira
                                    gold
                                    stocks
                                    commodities
                                    int-term-bonds
                                    long-term-bonds])
        withdrawal 10000M
        adjustments (accounts/allocate ira all-accounts {:withdrawal withdrawal})]
    (is (= [{:account gold
             :target-percentage 7.5M
             :target-value 34361.45625M
             :current-percentage 0.0749M ; TODO: multiply this by 100 also?
             :current-value 34309.80M
             :adj-value 0M}
            {:account stocks
             :target-percentage 30M
             :target-value 137445.825M
             :current-percentage 0.305M
             :current-value 139853.78M
             :adj-value -2400M}
            {:account commodities
             :target-percentage 7.5M
             :target-value 34361.45625M
             :current-percentage 0.0836M
             :current-value 38316.93M
             :adj-value -4000M}
            {:account int-term-bonds
             :target-percentage 15M
             :target-value 68722.9125M
             :current-percentage 0.151M
             :current-value 69167.10M
             :adj-value -400M}
            {:account long-term-bonds
             :target-percentage 40M
             :target-value 183261.100M
             :current-percentage 0.407M
             :current-value 186505.13M
             :adj-value -3200M}]
           (sort-by (comp :id :account) adjustments)))
    (is (= (- 0 withdrawal)
           (->> adjustments
                (map :adj-value)
                (reduce +)))
        "The net change is zero")))

(deftest find-account-by-path
  (let [accounts (map #(hash-map :path %) [["Investments"]
                                           ["Investments" "IRA"]
                                           ["Investments" "Eli's College"]
                                           ["Investments" "Fidelity"]
                                           ["Investments" "Fidelity"]
                                           ["Receivable" "Eli"]])]

    (is (= [["Investments" "Eli's College"]
            ["Investments" "Fidelity"]
            ["Investments" "Fidelity"]
            ["Receivable" "Eli"]]
           (map :path (accounts/find-by-path "Eli" accounts))))
    (is (= [["Receivable" "Eli"]]
           (map :path (accounts/find-by-path "Rec/Eli" accounts))))
    (is (= [["Receivable" "Eli"]]
           (map :path (accounts/find-by-path "Rec:Eli" accounts))))))
