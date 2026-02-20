(ns clj-money.entities.memo-ledger-entries-test
  (:require [clojure.test :refer [is]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test_assertions]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-account
                                            find-commodity
                                            find-lot]]
            [clj-money.entity-helpers :refer [assert-invalid
                                             assert-created]]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private entry-context
  [(factory :user {:user/email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:commodity{:name "Apple"
               :entity "Personal"
               :symbol "AAPL"
               :exchange :nasdaq
               :type :stock}
   #:account{:name "IRA"
             :entity "Personal"
             :type :asset
             :commodity "USD"}
   #:lot{:account "IRA"
         :commodity "AAPL"
         :purchase-price 150M
         :shares-purchased 10M
         :purchase-date (t/local-date 2020 1 15)}])

(defn- attributes []
  #:memo-ledger-entry{:lot (find-lot ["IRA" "AAPL"])
                      :transaction-date (t/local-date 2021 6 1)
                      :memo "2-for-1 stock split"})

(dbtest create-a-memo-ledger-entry
  (with-context entry-context
    (assert-created (attributes) :refs [:memo-ledger-entry/lot])))

(dbtest lot-is-required
  (with-context entry-context
    (assert-invalid (dissoc (attributes) :memo-ledger-entry/lot)
                    {:memo-ledger-entry/lot ["Lot is required"]})))

(dbtest transaction-date-is-required
  (with-context entry-context
    (assert-invalid (dissoc (attributes) :memo-ledger-entry/transaction-date)
                    {:memo-ledger-entry/transaction-date
                     ["Transaction date is required"]})))

(dbtest transaction-date-must-be-a-date
  (with-context entry-context
    (assert-invalid
      (assoc (attributes) :memo-ledger-entry/transaction-date "not-a-date")
      {:memo-ledger-entry/transaction-date
       ["Transaction date is invalid"]})))

(dbtest memo-is-required
  (with-context entry-context
    (assert-invalid (dissoc (attributes) :memo-ledger-entry/memo)
                    {:memo-ledger-entry/memo ["Memo is required"]})))

(def ^:private existing-entry-context
  (conj entry-context
        #:memo-ledger-entry{:lot ["IRA" "AAPL"]
                            :transaction-date (t/local-date 2021 6 1)
                            :memo "2-for-1 stock split"}))

(dbtest search-by-lot
  (with-context existing-entry-context
    (let [lot (find-lot ["IRA" "AAPL"])]
      (is (seq-of-maps-like?
            [#:memo-ledger-entry{:lot (util/->entity-ref lot)
                                 :transaction-date (t/local-date 2021 6 1)
                                 :memo "2-for-1 stock split"}]
            (entities/select {:memo-ledger-entry/lot lot}))))))
