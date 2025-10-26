(ns clj-money.entities.transaction-items-test
  (:require [clojure.test :refer [is]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.test-helpers :refer [dbtest]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            basic-context]]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.transaction-items :as trx-items]))

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

(dbtest realize-trx-item-accounts
  (with-context context
    (is (= #{"Checking" "Salary" "Groceries"}
                           (->> (entities/select
                                  (util/entity-type
                                    {:transaction/entity (find-entity "Personal")}
                                    :transaction-item)
                                  {:sort [[:transaction-item/index :asc]]})
                                (trx-items/realize-accounts)
                                (map (comp :account/name
                                           :transaction-item/account))
                                set)))))

