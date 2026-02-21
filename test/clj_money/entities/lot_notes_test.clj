(ns clj-money.entities.lot-notes-test
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
  #:lot-note{:lots [(util/->entity-ref (find-lot ["IRA" "AAPL"]))]
             :transaction-date (t/local-date 2021 6 1)
             :memo "2-for-1 stock split"})

(dbtest create-a-lot-note
  (with-context entry-context
    (assert-created (attributes))))

(dbtest lots-is-required
  (with-context entry-context
    (assert-invalid (dissoc (attributes) :lot-note/lots)
                    {:lot-note/lots ["Lots is required"]})))

(dbtest transaction-date-is-required
  (with-context entry-context
    (assert-invalid (dissoc (attributes) :lot-note/transaction-date)
                    {:lot-note/transaction-date
                     ["Transaction date is required"]})))

(dbtest transaction-date-must-be-a-date
  (with-context entry-context
    (assert-invalid
      (assoc (attributes) :lot-note/transaction-date "not-a-date")
      {:lot-note/transaction-date
       ["Transaction date is invalid"]})))

(dbtest memo-is-required
  (with-context entry-context
    (assert-invalid (dissoc (attributes) :lot-note/memo)
                    {:lot-note/memo ["Memo is required"]})))

(def ^:private existing-entry-context
  (conj entry-context
        #:lot-note{:lots [["IRA" "AAPL"]]
                   :transaction-date (t/local-date 2021 6 1)
                   :memo "2-for-1 stock split"}))

(dbtest search-by-memo
  (with-context existing-entry-context
    (let [lot (find-lot ["IRA" "AAPL"])]
      (is (seq-of-maps-like?
            [#:lot-note{:lots [(util/->entity-ref lot)]
                        :transaction-date (t/local-date 2021 6 1)
                        :memo "2-for-1 stock split"}]
            (entities/select {:lot-note/memo "2-for-1 stock split"}))))))
