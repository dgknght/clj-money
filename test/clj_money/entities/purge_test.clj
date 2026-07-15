(ns clj-money.entities.purge-test
  (:require [clojure.test :refer [is]]
            [clojure.java.io :as io]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.images.sql]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-price
                                            find-budget-item
                                            find-reconciliation
                                            find-image
                                            find-attachment]]
            [clj-money.entities :as entities]
            [clj-money.entities.purge :as purge]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private purge-context
  (conj basic-context
        #:commodity{:entity "Personal"
                    :type :stock
                    :name "Apple, Inc."
                    :symbol "AAPL"
                    :exchange :nasdaq}
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2017 3 2)
                :value 12.34M}
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:image{:user "john@doe.com"
                :original-filename "receipt.jpg"
                :content-type "image/jpg"
                :content (io/file (io/resource "fixtures/attachment.jpg"))}
        #:attachment{:caption "Receipt"
                     :transaction [(t/local-date 2017 1 1) "Paycheck"]
                     :image "receipt.jpg"}
        #:budget{:name "2017"
                 :entity "Personal"
                 :start-date (t/local-date 2017 1 1)
                 :period [1 :month]
                 :items [#:budget-item{:account "Groceries"
                                       :periods (repeat 1 50M)}]}
        #:scheduled-transaction{:entity "Personal"
                                :description "Scheduled Paycheck"
                                :start-date (t/local-date 2017 1 1)
                                :date-spec {:day 1}
                                :period [1 :month]
                                :items [#:scheduled-transaction-item{:action :debit
                                                                     :account "Checking"
                                                                     :quantity 900M}
                                        #:scheduled-transaction-item{:action :credit
                                                                     :account "Salary"
                                                                     :quantity 900M}]}
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 31)
                         :balance 1000M
                         :status :new
                         :items [[(t/local-date 2017 1 1) 1000M]]}))

; Datomic performs :db/excise asynchronously in the background, so this test
; can only assert that the normal (non-history) query no longer returns the
; purged data - it cannot assert that history has actually been purged.
(dbtest purge-completely-removes-an-entity-and-its-dependents
  (with-context purge-context
    (let [entity (find-entity "Personal")
          image (find-image "receipt.jpg")
          business (find-entity "Business")]
      (purge/purge-entity! entity)

      (is (nil? (entities/find entity))
          "The entity itself is removed")
      (is (empty? (entities/select {:account/entity entity}))
          "The entity's accounts are removed")
      (is (empty? (entities/select {:commodity/entity entity}))
          "The entity's commodities are removed")
      (is (nil? (entities/find (find-price ["AAPL" (t/local-date 2017 3 2)])))
          "Prices for the entity's commodities are removed")
      (is (empty? (entities/select {:transaction/entity entity}))
          "The entity's transactions are removed")
      (is (nil? (entities/find (find-attachment "Receipt")))
          "Attachments on the entity's transactions are removed")
      (is (empty? (entities/select {:budget/entity entity}))
          "The entity's budgets are removed")
      (is (nil? (entities/find (find-budget-item ["2017" "Groceries"])))
          "The entity's budget items are removed")
      (is (empty? (entities/select {:scheduled-transaction/entity entity}))
          "The entity's scheduled transactions are removed")
      (is (nil? (entities/find (find-reconciliation ["Checking" (t/local-date 2017 1 31)])))
          "The entity's reconciliations are removed")

      (is (some? (entities/find image))
          "A shared, user-owned image is not removed along with an attachment")

      (is (some? (entities/find business))
          "An unrelated entity belonging to the same user is not removed")
      (is (seq (entities/select {:account/entity business}))
          "An unrelated entity's accounts are not removed")
      (is (seq (entities/select {:commodity/entity business}))
          "An unrelated entity's commodities are not removed"))))
