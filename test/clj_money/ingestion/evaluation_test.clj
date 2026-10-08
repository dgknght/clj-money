(ns clj-money.ingestion.evaluation-test
  (:require [clojure.test :refer [deftest is testing]]
            [java-time.api :as t]
            [clj-money.ingestion.evaluation :as ev]))

(def ^:private expected
  {:location-name "Didi's Downtown"
   :location-address "7210 Main Street, Frisco, TX 75034"
   :date "2026-09-23"
   :total 35.78M
   :tax 2.28M
   :tax-rate nil
   :tip 6.00M
   :payment-account "Discover"
   :line-items [{:description "1/2 Buffalo Style Fried Cauliflower" :amount 7.00M :account "Dining"}
                {:description "1/2 Hot Honey Buttered Fried Chicken" :amount 8.00M :account "Dining"}
                {:description "1/2 Salisbury Steak" :amount 6.50M :account "Dining"}
                {:description "1/2 Slice Hot Fudge Pie" :amount 6.00M :account "Dining"}]})

(def ^:private perfect
  {:location-name "DIDI'S DOWNTOWN"
   :location-address "7210 Main St, Frisco, TX 75034"
   :date "09/23/26"
   :total 35.78
   :tax 2.28
   :tip 6.0
   :payment-account "Discover"
   :line-items [{:description "Buffalo Style Fried Cauliflower" :amount 7.0 :account "Dining"}
                {:description "Hot Honey Buttered Fried Chicken" :amount 8.0 :account "Dining"}
                {:description "Salisbury Steak" :amount 6.5 :account "Dining"}
                {:description "Hot Fudge Pie" :amount 6.0 :account "Dining"}
                {:description "Water" :amount 0.0 :account "Dining"}]})

(deftest parse-receipt-dates
  (is (= (t/local-date 2026 9 23) (ev/parse-date "2026-09-23")))
  (is (= (t/local-date 2026 9 23) (ev/parse-date "9/23/26")))
  (is (= (t/local-date 2026 9 23) (ev/parse-date "09-23-2026")))
  (is (= (t/local-date 2026 9 23) (ev/parse-date "2026-09-23T10:00:00Z")))
  (is (nil? (ev/parse-date "unknown")))
  (is (nil? (ev/parse-date nil))))

(deftest match-merchant-names
  (is (ev/name= "CAVA" "CAVA Plano"))
  (is (ev/name= "Trader Joe's" "TRADER JOES"))
  (is (not (ev/name= "Kroger" "New York")))
  (is (not (ev/name= "Kroger" nil))))

(deftest score-a-perfect-response
  (let [result (ev/score expected perfect)]
    (is (= 1.0 (:score result)) "The score is 100%")
    (is (:usable result) "The response is usable")
    (is (:consistent result) "The items, tax, and tip add up to the total")
    (is (empty? (:hallucinated result)) "Nothing is hallucinated")
    (is (nil? (:tax-rate result))
        "A value not on the receipt isn't scored")))

(deftest score-a-hallucinated-response
  (let [result (ev/score expected {:location-name "New York"
                                   :location-address "123 Main St, New York, NY 10001"
                                   :date "2025-04-07T10:00:00Z"
                                   :total 1000000
                                   :tax 0.05
                                   :tax-rate 0.05
                                   :payment-account "account_1234567890"
                                   :line-items [{:description "Widget"
                                                 :amount 500000
                                                 :account "account_1234567890"}]})]
    (is (= 0.0 (:score result)))
    (is (not (:usable result)))
    (is (not (:consistent result)))
    (is (= [:tax-rate] (:hallucinated result))
        "A value supplied when the receipt has none is a hallucination")))

(deftest score-line-items
  (testing "missing and extra items"
    (let [result (ev/score-items (:line-items expected)
                                 [{:amount 7.0 :account "Dining"}
                                  {:amount 8.0 :account "Groceries/Food"}
                                  {:amount 99.0 :account "Dining"}])]
      (is (< (abs (- 2/3 (:item-precision result))) 0.001))
      (is (= 0.5 (:item-recall result)))
      (is (= 0.25 (:account-accuracy result))
          "Only correctly matched items with the right account count")))
  (testing "a receipt with no items"
    (is (= 1.0 (:item-f1 (ev/score-items [] []))))
    (is (= 0.0 (:item-f1 (ev/score-items [] [{:amount 5.0}])))))
  (testing "an account with several acceptable answers"
    (is (= 1.0 (:account-accuracy
                 (ev/score-items [{:amount 4.99M
                                   :account #{"Groceries/Non-food"
                                              "Household/Misc Household"}}]
                                 [{:amount 4.99
                                   :account "Household/Misc Household"}]))))))

(deftest a-tax-rate-may-be-a-percentage
  (is (true? (:tax-rate (ev/score (assoc expected :tax-rate 0.0825M)
                                  (assoc perfect :tax-rate 8.25))))))
