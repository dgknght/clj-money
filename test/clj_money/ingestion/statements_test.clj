(ns clj-money.ingestion.statements-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account]]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.ingestion.statements :as stmts]))

(use-fixtures :each reset-db)

(def ^:private ctx
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "Please001!"
          :roles #{:user}}
   #:entity{:name "Personal"
            :user "john@doe.com"
            :settings #:settings{:expense-hints ["Prefer \"Dining\" for restaurants"]}}
   #:commodity{:name "US Dollar"
               :type :currency
               :symbol "USD"
               :entity "Personal"}
   #:account{:name "Opening Balances"
             :type :equity
             :entity "Personal"}
   #:account{:name "Checking"
             :type :asset
             :entity "Personal"}
   #:account{:name "Credit Card"
             :type :liability
             :entity "Personal"}
   #:account{:name "Salary"
             :type :income
             :entity "Personal"}
   #:account{:name "Household"
             :type :expense
             :entity "Personal"}
   #:account{:name "Dining"
             :type :expense
             :entity "Personal"
             :parent "Household"}
   #:account{:name "Groceries"
             :type :expense
             :entity "Personal"
             :parent "Household"}])

(deftest get-the-account-options
  (with-context ctx
    (let [entity (find-entity "Personal")]
      (testing "when the account is not known"
        (let [{:keys [statement-accounts category-accounts]} (stmts/account-options entity)]
          (is (= #{"Checking" "Credit Card"}
                 (set statement-accounts))
              "The asset and liability accounts are offered for the statement")
          (is (= #{"Checking" "Credit Card" "Salary" "Household/Dining" "Household/Groceries"}
                 (set category-accounts))
              "Leaf accounts other than equity are offered for the other side")))
      (testing "when the account is known"
        (is (nil? (:statement-accounts
                    (stmts/account-options entity
                                           :account (find-account "Checking"))))
            "The model isn't asked to identify the account")))))

(deftest the-schema-constrains-the-account-choices
  (let [schema (stmts/schema {:statement-accounts ["Checking"]
                              :category-accounts ["Groceries"]})]
    (is (= ["Checking"]
           (get-in schema [:properties :account :enum])))
    (is (= ["Groceries"]
           (get-in schema [:properties :transactions :items :properties :category :enum])))
    (is (some #{"account"} (:required schema))))
  (let [schema (stmts/schema {:category-accounts ["Groceries"]})]
    (is (nil? (get-in schema [:properties :account]))
        "The account is left out when it's already known")
    (is (not-any? #{"account"} (:required schema)))))

(deftest the-prompt-explains-the-sign-convention-and-includes-hints
  (let [prompt (stmts/prompt {:entity/settings
                              {:settings/expense-hints ["Prefer \"Dining\" for restaurants"]}})]
    (is (str/includes? prompt "signed by its effect on the balance"))
    (is (str/includes? prompt "- Prefer \"Dining\" for restaurants"))))

(deftest normalize-a-statement
  (is (= {:period-start (t/local-date 2026 8 20)
          :period-end (t/local-date 2026 9 18)
          :opening-balance 10M
          :closing-balance 5M
          :transactions [{:date (t/local-date 2026 9 1)
                          :amount -5M
                          :description "Rent"}]}
         (stmts/normalize {:period-start "2026-08-20"
                           :period-end "2026-09-18"
                           :opening-balance 10M
                           :closing-balance 5M
                           :transactions [{:date "2026-09-01"
                                           :amount -5M
                                           :description "Rent"}]}))))

(defn- item
  [id date quantity description]
  {:id id
   :transaction/transaction-date date
   :transaction/description description
   :transaction-item/polarized-quantity quantity})

(defn- line
  [date amount description]
  {:date date :amount amount :description description})

(deftest match-statement-lines-to-items
  (let [sep (partial t/local-date 2026 9)]
    (testing "an exact match"
      (is (= {:matched [[(line (sep 1) -10M "Rent") (item 1 (sep 1) -10M "Rent")]]
              :unmatched []}
             (stmts/match-items [(line (sep 1) -10M "Rent")]
                                [(item 1 (sep 1) -10.00M "Rent")]))
          "Amounts match regardless of scale"))
    (testing "a match within the date window"
      (is (= 1 (count (:matched (stmts/match-items [(line (sep 8) -42.17M "Hardware")]
                                                   [(item 1 (sep 5) -42.17M "Hardware")]))))))
    (testing "no match outside the date window"
      (is (= [(line (sep 20) -42.17M "Hardware")]
             (:unmatched (stmts/match-items [(line (sep 20) -42.17M "Hardware")]
                                            [(item 1 (sep 5) -42.17M "Hardware")])))))
    (testing "no match for a different amount, or the opposite sign"
      (is (empty? (:matched (stmts/match-items [(line (sep 1) -10M "Rent")]
                                               [(item 1 (sep 1) -11M "Rent")
                                                (item 2 (sep 1) 10M "Rent")])))))
    (testing "the same amount twice"
      (is (= #{[(sep 3) 3] [(sep 16) 16]}
             (->> (stmts/match-items [(line (sep 16) -75M "Zelle")
                                      (line (sep 3) -75M "Zelle")]
                                     [(item 16 (sep 16) -75M "Jordan")
                                      (item 3 (sep 3) -75M "Jordan")])
                  :matched
                  (map (fn [[l i]] [(:date l) (:id i)]))
                  set))
          "Each line takes the item nearest its date, and each item is used once"))
    (testing "a tie on the date"
      (is (= 2 (:id (second (first (:matched
                                     (stmts/match-items [(line (sep 1) -10M "Fresh Market")]
                                                        [(item 1 (sep 1) -10M "Shell")
                                                         (item 2 (sep 1) -10M "Fresh Market")]))))))
          "The more similar description wins"))))

(deftest make-a-transaction-from-a-statement-line
  (with-context ctx
    (let [entity (find-entity "Personal")
          by-path (stmts/accounts-by-path entity)
          checking (find-account "Checking")
          card (find-account "Credit Card")
          side (fn [trx account]
                 (->> (:transaction/items trx)
                      (filter #(= (:id account) (:id (:transaction-item/account %))))
                      (map (juxt :transaction-item/action :transaction-item/quantity))
                      first))]
      (testing "money leaving a bank account"
        (let [trx (stmts/make-trx (line (t/local-date 2026 9 1) -10M "Fresh Market")
                                  checking entity by-path)]
          (is (comparable? #:transaction{:transaction-date (t/local-date 2026 9 1)
                                         :description "Fresh Market"
                                         :source :ingestion
                                         :review-status :pending}
                           trx))
          (is (= [:credit 10M] (side trx checking)))))
      (testing "a charge on a credit card"
        (let [trx (stmts/make-trx (assoc (line (t/local-date 2026 9 1) 25M "Bella Napoli")
                                         :category "Household/Dining")
                                  card entity by-path)]
          (is (= [:credit 25M] (side trx card))
              "The charge increases the card's balance")
          (is (= [:debit 25M] (side trx (find-account "Dining")))
              "The category chosen by the model is used")))
      (testing "an unknown category"
        (let [deposit (stmts/make-trx (assoc (line (t/local-date 2026 9 1) 100M "Payroll")
                                             :category "Bonus")
                                      checking entity by-path)
              purchase (stmts/make-trx (assoc (line (t/local-date 2026 9 1) -5M "Shop")
                                              :category "Checking")
                                       checking entity by-path)]
          (is (= [:credit 100M] (side deposit (find-account "Salary")))
              "A deposit defaults to an income account")
          (is (= :debit (first (side purchase (find-account "Dining"))))
              "A withdrawal defaults to an expense account, never the statement's own"))))))
