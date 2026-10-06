(ns clj-money.ingestion.receipts-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-accounts]]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.ingestion.receipts :as rcpts]))

(use-fixtures :each reset-db)

(def ^:private groceries-receipt
  {:date "09-27-2026",
   :location_name "Trader Joe's",
   :location_address "2400 Preston Rd, Plano, TX 75093",
   :total 58.3,
   :tax 1.4,
   :payment_account "Discover",
   :line_items
   [{:description "BARS GRANOLA ABC ALMOND",
     :amount 3.99,
     :account "Groceries/Non-food"}
    {:description "HOLY OAT BITES PUMPKIN",
     :amount 3.99,
     :account "Groceries/Non-food"}
    {:description "MIDNIGHT MOON CHOCOLATES",
     :amount 3.99,
     :account "Groceries/Non-food"}
    {:description "EV OLIVE OIL SPANIS",
     :amount 8.49,
     :account "Groceries/Non-food"}
    {:description "T CHERRIES DK CHOCOLATE",
     :amount 7.99,
     :account "Groceries/Non-food"}
    {:description "COFFEE THREE KEYS THE QU",
     :amount 10.99,
     :account "Groceries/Non-food"}
    {:description "PLANTAIN CHIPS",
     :amount 1.99,
     :account "Groceries/Non-food"}
    {:description "PLANTAIN CHIPS",
     :amount 1.99,
     :account "Groceries/Non-food"}
    {:description "EV OLIVE OIL 100% SPANIS",
     :amount 8.49,
     :account "Groceries/Non-food"}
    {:description "T CARNATION BUNCH",
     :amount 4.99,
     :account "Groceries/Non-food"}]})

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

(deftest make-a-transaction-from-a-grocery-receipt
  (with-context ctx
    (let [entity (find-entity "Personal")
          [food non-food discover] (find-accounts "Food"
                                                  "Non-food"
                                                  "Discover")
          trx (rcpts/make-trx groceries-receipt entity)]
      (is (= #:transaction{:transaction-date (t/local-date 2026 9 27)
                           :description "Trader Joe's"}
             trx)
          "The transaction attributes are extracted from the receipt.")
      (is (= #{#:transaction-item{:account {:id (:id food)}
                                  :action :debit
                                  :quantity 52.9M}
               #:transaction-item{:account {:id (:id non-food)}
                                  :action :debit
                                  :quantity 5.4M}
               #:transaction-item{:account {:id (:id discover)}
                                  :action :credit
                                  :quantity 58.3M}}
             (-> trx :transaction/items set))
          "The the receipt items are aggregated into transaction items."))))
