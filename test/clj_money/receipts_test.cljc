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
                                                  :quantity 100M}
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
                                                 :quantity 100M}
                              #:transaction-item{:action :debit
                                                 :account {:id :groceries}
                                                 :quantity 100M
                                                 :memo "weekly stuff"}]}
         (receipts/->transaction #:receipt{:transaction-date (dates/local-date "2020-01-01")
                                           :transaction-id "abc123"
                                           :description "Kroger"
                                           :payment-account {:id :checking}
                                           :items [#:receipt-item{:account {:id :groceries}
                                                                  :quantity 100M
                                                                  :memo "weekly stuff"}]}))
      "A transaction id is preserved during the conversion")
  (is (= #:transaction{:transaction-date (dates/local-date "2020-01-01")
                       :description "Kroger"
                       :items [#:transaction-item{:action :debit
                                                  :account {:id :checking}
                                                  :quantity 10M}
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
                                                  :quantity 100M}
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
      "Empty items are removed"))

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
