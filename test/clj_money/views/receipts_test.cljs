(ns clj-money.views.receipts-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.views.receipts :as receipts]))

(def ^:private transaction
  #:transaction{:description "Kroger"
                :items [#:transaction-item{:action :credit
                                           :account {:id :checking}
                                           :quantity 100M
                                           :memo "cash back"}
                        #:transaction-item{:action :debit
                                           :account {:id :groceries}
                                           :quantity 80M
                                           :memo "weekly stuff"}
                        #:transaction-item{:action :debit
                                           :account {:id :household}
                                           :quantity 20M
                                           :memo nil}]})

(deftest extract-the-payment-account-and-items-for-reuse
  (is (= #:receipt{:payment-account {:id :checking}
                   :payment-memo "cash back"
                   :items [#:receipt-item{:account {:id :groceries}
                                          :quantity 80M
                                          :memo "weekly stuff"}
                           #:receipt-item{:account {:id :household}
                                          :quantity 20M
                                          :memo nil}]}
         (receipts/->reused-fields transaction))))

(deftest update-the-attachment-count-for-a-transaction
  (is (= [{:id 201 :transaction/attachment-count 1}
          {:id 202}]
         (:transactions (receipts/update-attachment-count
                          {:transactions [{:id 201 :transaction/attachment-count 2}
                                          {:id 202}]}
                          {:id 201}
                          dec)))
      "Only the matching transaction is updated"))
