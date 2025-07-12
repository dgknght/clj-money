(ns clj-money.accounts-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [dgknght.app-lib.test-assertions]
            [clj-money.decimal :refer [d] :as dec]
            [clj-money.accounts :as accounts]))

(deftest create-criteria-from-one-account
  (is (= {:transaction/transaction-date [:between
                                              (t/local-date 2020 1 1)
                                              (t/local-date 2020 12 31)]
          :transaction-item/account {:id 101}}
         (accounts/->criteria
           {:id 101
            :account/transaction-date-range [(t/local-date 2020 1 1)
                                             (t/local-date 2020 12 31)]}))))

(deftest create-criteria-from-multiple-accounts
  (is (= {:transaction/transaction-date [:between
                                              (t/local-date 2020 1 1)
                                              (t/local-date 2020 2 29)]
          :transaction-item/account {:id [:in #{101 102}]}}
         (accounts/->>criteria
           [{:id 101
             :account/transaction-date-range [(t/local-date 2020 2 1)
                                              (t/local-date 2020 2 29)]}
            {:id 102
             :account/transaction-date-range [(t/local-date 2020 1 1)
                                              (t/local-date 2020 1 31)]}]))))

(deftest derive-an-item
  (testing "from a positive quantity"
    (let [quantity (d 10)]
      (is (= #:transaction-item{:quantity (d 10) :action :debit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :asset}}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity (d 10) :action :credit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :liability}}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity (d 10) :action :credit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :equity}}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity (d 10) :action :credit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :income}}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity (d 10) :action :debit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :expense}}))
          "The action is :debit")))
  (testing "from a negative quantity"
    (let [quantity (d -10)]
      (is (= #:transaction-item{:quantity (d 10) :action :credit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :asset}}))
          "The action is :credit")
      (is (= #:transaction-item{:quantity (d 10) :action :debit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :liability}}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity (d 10) :action :debit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :equity}}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity (d 10) :action :debit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :income}}))
          "The action is :debit")
      (is (= #:transaction-item{:quantity (d 10) :action :credit :account {:id 1}}
             (accounts/->transaction-item
               {:quantity quantity
                :account {:id 1 :account/type :expense}}))
          "The action is :credit")))
  (testing "with only a quantity (no account)"
    (is (= {:transaction-item/quantity (d 10)
            :transaction-item/action :credit}
           (accounts/->transaction-item {:quantity (d 10)}))))
  (testing "with only an account (no quantity)"
    (is (= {:transaction-item/account {:id 1}
            :transaction-item/action :credit}
           (accounts/->transaction-item {:account {:id 1}})))))

(def ^:private flat-accounts
  [{:id :savings
    :account/type :asset
    :account/name "Savings"
    :account/value (d 1)}
   {:id :savings-car
    :account/parent {:id :savings}
    :account/type :asset
    :account/name "Car"
    :account/value (d 1000)}
   {:id :savings-reserve
    :account/parent {:id :savings}
    :account/type :asset
    :account/name "Reserve"
    :account/value  (d 2000)}])

(def ^:private nested-accounts
  [{:type :asset
    :accounts
    [{:id :savings
      :account/type :asset
      :account/name "Savings"
      :account/has-children? true
      :account/children-value (d 3000)
      :account/total-value (d 3001)
      :account/value (d 1)
      :account/children [{:id :savings-car
                          :account/parent {:id :savings}
                          :account/type :asset
                          :account/name "Car"
                          :account/value (d 1000)
                          :account/total-value (d 1000)}
                         {:id :savings-reserve
                          :account/parent {:id :savings}
                          :account/type :asset
                          :account/name "Reserve"
                          :account/value  (d 2000)
                          :account/total-value (d 2000)}]}]}
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
                        :account/quantity (d 468931.04)
                        :account/total-value (d 468931.04)
                        :account/value (d 778.29)
                        :account/allocations {:gold (d 7.5)
                                              :stocks (d 30)
                                              :commodities (d 7.5)
                                              :int-term-bonds (d 15)
                                              :long-term-bonds (d 40)}}
                       {:id :gold
                        :account/name "Gold"
                        :account/value (d 34309.80)}
                       {:id :stocks
                        :account/name "Stocks"
                        :account/value (d 139853.78)}
                       {:id :commodities
                        :account/name "Commodities"
                        :account/value (d 38316.93)}
                       {:id :int-term-bonds
                        :account/name "Intermediate Term Bods"
                        :account/value (d 69167.10)}
                       {:id :long-term-bonds
                        :account/name "Long Term Bods"
                        :account/value (d 186505.13)}]
                      (map (juxt :id identity))
                      (into {}))
        adjustments (accounts/allocate (accounts :ira)
                                            accounts)]
    (is (= [[:commodities (d -3200)]
            [:gold (d 700)]
            [:int-term-bonds (d 1100)]
            [:long-term-bonds (d 800)]
            [:stocks (d 600)]]
           (->> adjustments
                (sort-by (comp :id :account))
                (map (juxt (comp :id :account) :adj-value))))
        "The adjusted values are included in the return")
    #_(is (= [{:account (accounts :commodities)
             :target-percentage (d 7.5)
             :target-value (d 35111.45625)
             :current-percentage (d 0.0818)
             :current-value (d 38316.93)
             :adj-value (d -3200)}
            {:account (accounts :gold)
             :target-percentage (d 7.5)
             :target-value (d 35111.45625)
             :current-percentage (d 0.0733)
             :current-value (d 34309.80)
             :adj-value (d 700)}
            {:account (accounts :int-term-bonds)
             :target-percentage (d 15)
             :target-value (d 70222.9125)
             :current-percentage (d 0.148)
             :current-value (d 69167.10)
             :adj-value (d 1100)}
            {:account (accounts :long-term-bonds)
             :target-percentage (d 40)
             :target-value (d 187261.10)
             :current-percentage (d 0.398)
             :current-value (d 186505.13)
             :adj-value (d 800)}
            {:account (accounts :stocks)
             :target-percentage (d 30)
             :target-value (d 140445.825)
             :current-percentage (d 0.299)
             :current-value (d 139853.78)
             :adj-value (d 600)}]
           (sort-by (comp :id :account) adjustments)))
    (is (dec/zero? (->> adjustments
                    (map :adj-value)
                    (reduce dec/+ (d 0))))
        "The net change is zero")))

(deftest reallocate-for-withdrawal
  (let [accounts (->> [{:id :ira
                        :account/name "IRA"
                        :account/quantity (d 468931.04)
                        :account/total-value (d 468931.04)
                        :account/value (d 778.29)
                        :account/allocations {:gold (d 7.5)
                                              :stocks (d 30)
                                              :commodities (d 7.5)
                                              :int-term-bonds (d 15)
                                              :long-term-bonds (d 40)}}
                       {:id :gold
                        :account/name "Gold"
                        :account/value (d 34309.80)}
                       {:id :stocks
                        :account/name "Stocks"
                        :account/value (d 139853.78)}
                       {:id :commodities
                        :account/name "Commodities"
                        :account/value (d 38316.93)}
                       {:id :int-term-bonds
                        :account/name "Intermediate Term Bods"
                        :account/value (d 69167.10)}
                       {:id :long-term-bonds
                        :account/name "Long Term Bods"
                        :account/value (d 186505.13)}]
                      (map (juxt :id identity))
                      (into {}))
        withdrawal (d 10000)
        adjustments (accounts/allocate (accounts :ira) accounts :withdrawal withdrawal)]
    (is (= [[:commodities (d -4000)]
            [:gold (d 0)]
            [:int-term-bonds (d -400)]
            [:long-term-bonds (d -3200)]
            [:stocks (d -2400)]]
           (->> adjustments
                (sort-by (comp :id :account))
                (map (juxt (comp :id :account) :adj-value))))
        "The adjustment values are included in the return")
    #_(is (= [{:account (accounts :commodities)
             :target-percentage (d 7.5)
             :target-value (d 34361.45625)
             :current-percentage (d 0.0836)
             :current-value (d 38316.93)
             :adj-value (d -4000)}
            {:account (accounts :gold)
             :target-percentage (d 7.5)
             :target-value (d 34361.45625)
             :current-percentage (d 0.0749) ; TODO: multiply this by 100 also?
             :current-value (d 34309.80)
             :adj-value (d 0)}
            {:account (accounts :int-term-bonds)
             :target-percentage (d 15)
             :target-value (d 68722.9125)
             :current-percentage (d 0.151)
             :current-value (d 69167.10)
             :adj-value (d -400)}
            {:account (accounts :long-term-bonds)
             :target-percentage (d 40)
             :target-value (d 183261.100)
             :current-percentage (d 0.407)
             :current-value (d 186505.13)
             :adj-value (d -3200)}
            {:account (accounts :stocks)
             :target-percentage (d 30)
             :target-value (d 137445.825)
             :current-percentage (d 0.305)
             :current-value (d 139853.78)
             :adj-value (d -2400)}]
           (sort-by (comp :id :account) adjustments)))
    (is (= (dec/- (d 0) withdrawal)
           (->> adjustments
                (map :adj-value)
                (reduce dec/+)))
        "The net change is the amount of the withdrawal")))

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
  (let [account {:account/type account-type}]
    (is (= expected
           (accounts/polarize-quantity quantity
                                       action
                                       account))
        message)))

(deftest polarize-a-quantity
  ; Debits
  (test-polarization :asset     :debit (d 100)  (d 100) "A debit in an asset account increases the balance")
  (test-polarization :expense   :debit (d 100)  (d 100) "A debit in an expense account increases the balance")
  (test-polarization :liability :debit (d 100) (d -100) "A debit in an liability account decreases the balance")
  (test-polarization :equity    :debit (d 100) (d -100) "A debit in an equity account decreases the balance")
  (test-polarization :income    :debit (d 100) (d -100) "A debit in an income account decreases the balance")

  ;; Credits
  (test-polarization :asset     :credit (d 100) (d -100) "A credit in an asset account decreases the balance")
  (test-polarization :expense   :credit (d 100) (d -100) "A credit in an expense account dereases the balance")
  (test-polarization :liability :credit (d 100)  (d 100) "A credit in an liability account increases the balance")
  (test-polarization :equity    :credit (d 100)  (d 100) "A credit in an equity account increases the balance")
  (test-polarization :income    :credit (d 100)  (d 100) "A credit in an income account increases the balance"))

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

(def ^:private standard-accounts
  [{:id :checking
    :account/name "Checking"
    :account/commodity {:id :usd}
    :account/type :asset}
   {:id :savings
    :account/name "Savings"
    :account/commodity {:id :usd}
    :account/type :asset}
   {:id :reserve
    :account/parent {:id :savings}
    :account/name "Reserve"
    :account/commodity {:id :usd}
    :account/type :asset}
   {:id :car
    :account/parent {:id :savings}
    :account/name "Car"
    :account/commodity {:id :usd}
    :account/type :asset}])

(def ^:private standard-supplemental-data
  {:balances {:checking (d 1000)
              :savings (d 0)
              :reserve (d 10000)
              :car (d 5000)}})

(def ^:private commodity-accounts
  [{:id :ira
    :account/name "IRA"
    :account/type :asset
    :account/system-tags #{:trading}
    :account/commodity {:id :usd}}
   {:id :ira-aapl
    :account/parent {:id :ira}
    :account/name "AAPL"
    :account/type :asset
    :account/system-tags #{:tradable}
    :account/commodity {:id :aapl}}
   {:id :ira-msft
    :account/parent {:id :ira}
    :account/name "MSFT"
    :account/type :asset
    :account/system-tags #{:tradable}
    :account/commodity {:id :msft}}
   {:id :four-o-one-k
    :account/name "401k"
    :account/type :asset
    :account/system-tags #{:trading}
    :account/commodity {:id :usd}}
   {:id :four-o-one-k-aapl
    :account/parent {:id :four-o-one-k}
    :account/name "AAPL"
    :account/type :asset
    :account/system-tags #{:tradable}
    :account/commodity {:id :aapl}}
   {:id :four-o-one-k-msft
    :account/parent {:id :four-o-one-k}
    :account/name "MSFT"
    :account/type :asset
    :account/system-tags #{:tradable}
    :account/commodity {:id :msft}}])

(def ^:private commodity-supplemental-data
  {:balances {:ira (d 1000)
              :four-o-one-k (d 2000)}
   :lots {[:ira :aapl] [{:id 300
                         :lot/purchase-date (t/local-date 2020 1 1)}]
          [:ira :msft] [{:id 301
                         :lot/purchase-date (t/local-date 2020 1 1)}]
          [:four-o-one-k :aapl] [{:id 302
                                  :lot/purchase-date (t/local-date 2020 1 1)}
                                 {:id 304
                                  :lot/purchase-date (t/local-date 2020 2 1)}]
          [:four-o-one-k :msft] [{:id 303
                                  :lot/purchase-date (t/local-date 2020 1 1)}]}
   :lot-items {300 [{:transaction/transaction-date (t/local-date 2020 1 1) ; AAPL in IRA
                     :lot-item/action :buy
                     :lot-item/shares (d 100)
                     :lot-item/price (d 10)}]
               301 [{:transaction/transaction-date (t/local-date 2020 1 1) ; MSFT in IRA
                     :lot-item/action :buy
                     :lot-item/shares (d 100)
                     :lot-item/price (d 10)}
                    {:transaction/transaction-date (t/local-date 2020 2 1)
                     :lot-item/action :sell
                     :lot-item/shares (d 50)
                     :lot-item/price (d 9)}]
               302 [{:transaction/transaction-date (t/local-date 2020 1 1); AAPL in 401k
                     :lot-item/action :buy
                     :lot-item/shares (d 100)
                     :lot-item/price (d 10)}]
               303 [{:transaction/transaction-date (t/local-date 2020 1 1) ; MSFT in 401k
                     :lot-item/action :buy
                     :lot-item/shares (d 100)
                     :lot-item/price (d 10)}]
               304 [{:transaction/transaction-date (t/local-date 2020 2 1) ; AAPL in 401k
                     :lot-item/action :buy
                     :lot-item/shares (d 100)
                     :lot-item/price (d 12)}]}
   :prices {:aapl (d 15)
            :msft (d 8)}})

(def ^:private lot-key
  (juxt (comp :id :account/parent)
        (comp :id :account/commodity)))

(defn- valuation-data
  [data]
  (reify accounts/ValuationData
    (fetch-entity [_ _]
      {:entity/settings {:settings/default-commodity {:id :usd}}})
    (fetch-balance
      [_ account]
      (get-in data [:balances (:id account)]))
    (fetch-lots
      [_ account]
      (get-in data [:lots (lot-key account)]))
    (fetch-lot-items
      [_ lot]
      (get-in data [:lot-items (:id lot)]))
    (fetch-price
      [_ commodity]
      (get-in data [:prices (:id commodity)]))))

(deftest valuate-some-accounts
  (testing "Simple accounts (with the default commodity)"
    (let [expected [{:account/name "Checking"
                     :account/value (d 1000)
                     :account/total-value (d 1000)}
                    {:account/name "Savings"
                     :account/value (d 0)
                     :account/children-value (d 15000)
                     :account/total-value (d 15000)}
                    {:account/name "Car"
                     :account/value (d 5000)
                     :account/total-value (d 5000)}
                    {:account/name "Reserve"
                     :account/value (d 10000)
                     :account/total-value (d 10000)}]
          actual (accounts/valuate
              (valuation-data standard-supplemental-data)
              standard-accounts)]
      #?(:clj (is (seq-of-maps-like? expected actual))
         :cljs (is (dgknght.app-lib.test-assertions/seq-of-maps-like? expected actual)))))
  (testing "Simple and commodity accounts"
    (let [expected [{:account/name "401k"
                     :account/value (d 2000)
                     :account/cost-basis (d 5200) ; (d 2000) cash + sum of children
                     :account/total-value (d 5800)
                     :account/gain (d 600)}
                    {:account/name "AAPL"
                     :account/shares-owned (d 200)
                     :account/cost-basis (d 2200)
                     :account/current-price (d 15)
                     :account/total-value (d 3000)
                     :account/gain (d 800)}
                    {:account/name "MSFT"
                     :account/shares-owned (d 100)
                     :account/cost-basis (d 1000)
                     :account/current-price (d 8)
                     :account/total-value (d 800)
                     :account/gain (d -200)}
                    {:account/name "IRA"
                     :account/value (d 1000)
                     :account/cost-basis (d 2500) ; (d 1000) cash + sum of children
                     :account/total-value (d 2900) ; (d 1500) of AAPL + (d 500) of MSFT + (d 1000) in cash
                     :account/gain (d 400)}
                    {:account/name "AAPL"
                     :account/shares-owned (d 100)
                     :account/cost-basis (d 1000)
                     :account/current-price (d 15)
                     :account/total-value (d 1500)
                     :account/gain (d 500)}
                    {:account/name "MSFT"
                     :account/shares-owned (d 50)
                     :account/cost-basis (d 500)
                     :account/current-price (d 8)
                     :account/total-value (d 400)
                     :account/gain (d -100)}]
          actual (accounts/valuate
                   (valuation-data commodity-supplemental-data)
                   commodity-accounts)]
      #?(:clj (is (seq-of-maps-like? expected actual))
         :cljs (is (dgknght.app-lib.test-assertions/seq-of-maps-like? expected actual))))))
