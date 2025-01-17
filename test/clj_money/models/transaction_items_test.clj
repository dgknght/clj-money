(ns clj-money.models.transaction-items-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            basic-context]]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.db :as db]
            [clj-money.models :as models]
            [clj-money.models.transaction-items :as trx-items]))

(use-fixtures :each reset-db)

(def ^:private context
  (conj basic-context
        #:transaction{:entity "Personal"
                      :description "Payment"
                      :transaction-date (t/local-date 2016 1 1)
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:entity "Personal"
                      :description "Kroger"
                      :transaction-date (t/local-date 2016 1 2)
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 100M}))

(deftest realize-trx-item-accounts
  (with-context context
    (is (= #{"Checking" "Salary" "Groceries"}
                           (->> (models/select
                                  (db/model-type
                                    {:transaction/entity (find-entity "Personal")}
                                    :transaction-item)
                                  {:sort [[:transaction-item/index :asc]]})
                                (trx-items/realize-accounts)
                                (map (comp :account/name
                                           :transaction-item/account))
                                set)))))

