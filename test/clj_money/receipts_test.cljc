(ns clj-money.receipts-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is]])
            [clojure.spec.alpha :as s]
            [clj-money.dates :as dates]
            [clj-money.receipts :as receipts]))

(deftest convert-a-receipt-to-a-transaction
  (is (= #:transaction{:transaction-date (dates/local-date "2020-01-01")
                       :description "Kroger"
                       :items [#:transaction-item{:action :credit
                                                  :account {:id :checking}
                                                  :quantity 100M
                                                  :memo nil}
                               #:transaction-item{:action :debit
                                                  :account {:id :groceries}
                                                  :quantity 100M
                                                  :memo "weekly stuff"}]}
         (receipts/->transaction #:receipt{:transaction-date (dates/local-date "2020-01-01")
                                           :description "Kroger"
                                           :payment-account {:id :checking}
                                           :items [#:receipt-item{:account {:id :groceries}
                                                                  :quantity 100M
                                                                  :memo "weekly stuff"}]}))
      "A payment is converted to a transaction")
  (is (= {:id "abc123"
          :transaction/transaction-date (dates/local-date "2020-01-01")
          :transaction/description "Kroger"
          :transaction/items [#:transaction-item{:action :credit
                                                 :account {:id :checking}
                                                 :quantity 100M
                                                 :memo "cash back"}
                              #:transaction-item{:action :debit
                                                 :account {:id :groceries}
                                                 :quantity 100M
                                                 :memo "weekly stuff"}]}
         (receipts/->transaction #:receipt{:transaction-date (dates/local-date "2020-01-01")
                                           :transaction-id "abc123"
                                           :description "Kroger"
                                           :payment-account {:id :checking}
                                           :payment-memo "cash back"
                                           :items [#:receipt-item{:account {:id :groceries}
                                                                  :quantity 100M
                                                                  :memo "weekly stuff"}]}))
      "A transaction id and payment memo are preserved during the conversion")
  (is (= #:transaction{:transaction-date (dates/local-date "2020-01-01")
                       :description "Kroger"
                       :items [#:transaction-item{:action :debit
                                                  :account {:id :checking}
                                                  :quantity 10M
                                                  :memo nil}
                               #:transaction-item{:action :credit
                                                  :account {:id :groceries}
                                                  :quantity 10M
                                                  :memo "the milk was spoiled"}]}
         (receipts/->transaction #:receipt{:transaction-date (dates/local-date "2020-01-01")
                                           :description "Kroger"
                                           :payment-account {:id :checking}
                                           :items [#:receipt-item{:account {:id :groceries}
                                                                  :quantity -10M
                                                                  :memo "the milk was spoiled"}]}))
      "A refund is converted to a transaction")
  (is (= #:transaction{:transaction-date (dates/local-date "2020-01-01")
                       :description "Kroger"
                       :items [#:transaction-item{:action :credit
                                                  :account {:id :checking}
                                                  :quantity 100M
                                                  :memo nil}
                               #:transaction-item{:action :debit
                                                  :account {:id :groceries}
                                                  :quantity 100M
                                                  :memo nil}]}
         (receipts/->transaction #:receipt{:transaction-date (dates/local-date "2020-01-01")
                                           :description "Kroger"
                                           :payment-account {:id :checking}
                                           :items [#:receipt-item{:account {:id :groceries}
                                                                  :quantity 100M}
                                                   {}]}))
      "Empty items are removed")
  (is (= {:id "abc123"
          :transaction/transaction-date (dates/local-date "2020-01-01")
          :transaction/description "Kroger"
          :transaction/items [{:id "payment-item-id"
                               :transaction-item/action :credit
                               :transaction-item/account {:id :checking}
                               :transaction-item/quantity 100M
                               :transaction-item/memo "cash back"}
                              {:id "expense-item-id"
                               :transaction-item/action :debit
                               :transaction-item/account {:id :groceries}
                               :transaction-item/quantity 100M
                               :transaction-item/memo "weekly stuff"}]}
         (receipts/->transaction #:receipt{:transaction-date (dates/local-date "2020-01-01")
                                           :transaction-id "abc123"
                                           :description "Kroger"
                                           :payment-account {:id :checking}
                                           :payment-id "payment-item-id"
                                           :payment-memo "cash back"
                                           :items [(assoc #:receipt-item{:account {:id :groceries}
                                                                         :quantity 100M
                                                                         :memo "weekly stuff"}
                                                          :receipt-item/id "expense-item-id")]}))
      "Item ids are preserved during the conversion so edits merge onto existing items"))

(deftest validation
  (is (s/valid? ::receipts/receipt
                #:receipt{:transaction-date (dates/local-date "2020-01-01")
                          :description "Kroger"
                          :payment-account {:id :checking}
                          :items [#:receipt-item{:account {:id :groceries}
                                                 :quantity 100M
                                                 :memo "weekly stuff"}]})
      "A receipt with valid data passes validation")
  (is (not
        (s/valid? ::receipts/receipt
                  #:receipt{:transaction-date (dates/local-date "2020-01-01")
                            :description "Kroger"
                            :payment-account {:id :checking}
                            :items [#:receipt-item{:account {:id :groceries}
                                                   :quantity 100M
                                                   :memo "weekly stuff"}
                                    #:receipt-item{:account {:id :household}
                                                   :quantity -100M
                                                   :memo "weekly stuff"}]}))
      "A receipt with mixed positive and negative item quantities is not valid"))

(deftest convert-a-transaction-to-a-receipt
  (is (= #:receipt{:transaction-date (dates/local-date "2020-01-01")
                   :transaction-id "abc123"
                   :description "Kroger"
                   :payment-account {:id :checking}
                   :payment-memo "cash back"
                   :items [#:receipt-item{:account {:id :groceries}
                                          :quantity 100M
                                          :memo "weekly stuff"}]}
         (receipts/<-transaction
           {:id "abc123"
            :transaction/transaction-date (dates/local-date "2020-01-01")
            :transaction/description "Kroger"
            :transaction/items [#:transaction-item{:action :credit
                                                   :account {:id :checking}
                                                   :quantity 100M
                                                   :memo "cash back"}
                                #:transaction-item{:action :debit
                                                   :account {:id :groceries}
                                                   :quantity 100M
                                                   :memo "weekly stuff"}]})))
  (is (= #:receipt{:transaction-date (dates/local-date "2020-01-01")
                   :transaction-id "abc123"
                   :description "Kroger"
                   :payment-account {:id :checking}
                   :payment-id "payment-item-id"
                   :payment-memo "cash back"
                   :items [#:receipt-item{:id "expense-item-id"
                                          :account {:id :groceries}
                                          :quantity 100M
                                          :memo "weekly stuff"}]}
         (receipts/<-transaction
           {:id "abc123"
            :transaction/transaction-date (dates/local-date "2020-01-01")
            :transaction/description "Kroger"
            :transaction/items [{:id "payment-item-id"
                                 :transaction-item/action :credit
                                 :transaction-item/account {:id :checking}
                                 :transaction-item/quantity 100M
                                 :transaction-item/memo "cash back"}
                                {:id "expense-item-id"
                                 :transaction-item/action :debit
                                 :transaction-item/account {:id :groceries}
                                 :transaction-item/quantity 100M
                                 :transaction-item/memo "weekly stuff"}]}))
      "Item ids are preserved so a subsequent edit can be merged onto the existing items"))

(deftest calculate-a-receipt-total
  (is (= 100M
         (receipts/total
           #:receipt{:items [#:receipt-item{:account {:id :groceries}
                                            :quantity 100M
                                            :memo "weekly stuff"}]}))
      "The total is the quantity of a receipt with one item")
  (is (= 100M
         (receipts/total
           #:receipt{:items [#:receipt-item{:account {:id :groceries}
                                            :quantity 100M
                                            :memo "weekly stuff"}
                             {}]}))
      "Empty items are ignored")
  (is (= 110M
         (receipts/total
           #:receipt{:items [#:receipt-item{:account {:id :groceries}
                                            :quantity 100M
                                            :memo "weekly stuff"}
                             #:receipt-item{:account {:id :medicine}
                                            :quantity 10M
                                            :memo "weekly stuff"}]}))
      "Multiple items are summed"))
