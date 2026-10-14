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

(def ^:private ingested-receipt
  #:receipt{:transaction-date "2015-01-02"
            :transaction-id 101
            :description "Kroger"
            :payment-account {:id :checking}
            :items [#:receipt-item{:account {:id :groceries}
                                   :quantity 10.5M}
                    {}]})

(deftest cancel-edits-to-an-ingested-receipt
  (let [page-state (atom {:ingestion {:id 201}
                          :ingested-receipt ingested-receipt
                          :receipt (assoc ingested-receipt
                                          :receipt/description "Kroger #123")})]
    (receipts/cancel-receipt page-state)
    (is (= ingested-receipt (:receipt @page-state))
        "The receipt is restored as read from the image")
    (is (= {:id 201} (:ingestion @page-state))
        "The ingestion is kept, so the transaction can still be accepted or rejected")))

(deftest cancel-a-receipt-entered-by-hand
  (let [page-state (atom {:receipt #:receipt{:transaction-date "2015-01-02"
                                             :description "Kroger"}})]
    (receipts/cancel-receipt page-state)
    (is (= #:receipt{:transaction-date "2015-01-02"}
           (:receipt @page-state))
        "Everything but the transaction date is cleared")))
