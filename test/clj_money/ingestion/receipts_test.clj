(ns clj-money.ingestion.receipts-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-accounts]]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.ingestion.receipts :as rcpts]))

(use-fixtures :each reset-db)

(def ^:private ctx
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "Please001!"
          :roles #{:user}}
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :type :currency
               :symbol "USD"
               :entity "Personal"}
   #:account{:name "Credit Cards"
             :type :liability
             :entity "Personal"}
   #:account{:name "Mastercard"
             :type :liability
             :parent "Credit Cards"
             :entity "Personal"}
   #:account{:name "Discover"
             :type :liability
             :parent "Credit Cards"
             :entity "Personal"}
   #:account{:name "Groceries"
             :type :expense
             :entity "Personal"}
   #:account{:name "Food"
             :type :expense
             :parent "Groceries"
             :entity "Personal"}
   #:account{:name "Non-food"
             :type :expense
             :parent "Groceries"
             :entity "Personal"}])

(def ^:private groceries-receipt
  {:date "09-27-2026"
   :location-name "Trader Joe's"
   :location-address "2400 Preston Rd, Plano, TX 75093"
   :total 58.3M
   :tax 1.4M
   :tax-rate 0.0825M
   :payment-account "Discover"
   :line-items
   [{:description "BARS GRANOLA ABC ALMOND"
     :amount 3.99M
     :account "Groceries/Food"}
    {:description "HOLY OAT BITES PUMPKIN"
     :amount 3.99M
     :taxable true
     :account "Groceries/Food"}
    {:description "MIDNIGHT MOON CHOCOLATES"
     :amount 3.99M
     :account "Groceries/Food"}
    {:description "EV OLIVE OIL SPANIS"
     :amount 8.49M
     :account "Groceries/Food"}
    {:description "T CHERRIES DK CHOCOLATE"
     :amount 7.99M
     :taxable true
     :account "Groceries/Food"}
    {:description "COFFEE THREE KEYS THE QU"
     :amount 10.99M
     :account "Groceries/Food"}
    {:description "PLANTAIN CHIPS"
     :amount 1.99M
     :account "Groceries/Food"}
    {:description "PLANTAIN CHIPS"
     :amount 1.99M
     :account "Groceries/Food"}
    {:description "EV OLIVE OIL 100% SPANIS"
     :amount 8.49M
     :account "Groceries/Food"}
    {:description "T CARNATION BUNCH"
     :amount 4.99M
     :taxable true
     :account "Groceries/Non-food"}]})

(deftest make-a-transaction-from-a-grocery-receipt
  (with-context ctx
    (let [entity (find-entity "Personal")
          [food non-food discover] (find-accounts "Food"
                                                  "Non-food"
                                                  "Discover")
          trx (rcpts/make-trx groceries-receipt entity)]
      (is (comparable? #:transaction{:transaction-date (t/local-date 2026 9 27)
                                     :description "Trader Joe's"}
                       trx)
          "The transaction attributes are extracted from the receipt.")
      (is (= 58.3M
             (->> (:transaction/items trx)
                  (filter #(= :debit (:transaction-item/action %)))
                  (map :transaction-item/quantity)
                  (reduce + 0M))
             (->> (:transaction/items trx)
                  (filter #(= :credit(:transaction-item/action %)))
                  (map :transaction-item/quantity)
                  (reduce + 0M)))
          "The transaction is balanced at a total equal to the receipt total")
      (let [expected-items #{#:transaction-item{:account (util/simplify food)
                                                :action :debit
                                                :quantity 52.9M}
                             #:transaction-item{:account (util/simplify non-food)
                                                :action :debit
                                                :quantity 5.4M}
                             #:transaction-item{:account (util/simplify discover)
                                                :action :credit
                                                :quantity 58.3M}}
            actual-items (->> (:transaction/items trx)
                              (map #(update-in %
                                               [:transaction-item/account]
                                               util/simplify))
                              set)
            [missing extra] (diff expected-items actual-items)]
        (when (or (seq missing) (seq extra))
          (pprint {:missing missing :extra extra}))
        (is (= expected-items actual-items)
            "The the receipt items are aggregated into transaction items.")))))
