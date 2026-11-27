(ns clj-money.api.transaction-exports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.data.csv :as csv]
            [java-time.api :as t]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.api.test-helper :refer [request]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [basic-context
                                            with-context
                                            find-account
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db dbtest]]
            [clj-money.web.test-handler :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :memo "first of the year"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date (t/local-date 2017 1 5)
                      :entity "Personal"
                      :description "Landlord"
                      :quantity 1000M
                      :debit-account "Rent"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2017 1 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :items [#:transaction-item{:action :credit
                                                 :account "Salary"
                                                 :quantity 1000M}
                              #:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 800M
                                                 :memo "net pay"}
                              #:transaction-item{:action :debit
                                                 :account "FIT"
                                                 :quantity 150M}
                              #:transaction-item{:action :debit
                                                 :account "Medicare"
                                                 :quantity 50M}]}
        #:transaction{:transaction-date (t/local-date 2017 1 20)
                      :entity "Personal"
                      :description "Kroger"
                      :quantity 100.25M
                      :debit-account "Groceries"
                      :credit-account "Checking"}
        #:reconciliation{:end-of-period (t/local-date 2017 1 10)
                         :account "Checking"
                         :items [[(t/local-date 2017 1 1) 1000M]
                                 [(t/local-date 2017 1 5) 1000M]]
                         :balance 0M
                         :status :completed}))

(defn- export
  [email & {:as params}]
  (let [res (-> (request :get (cond-> (path :api
                                            :accounts
                                            (:id (find-account "Checking"))
                                            :transaction-items
                                            :export)
                                (seq params) (str "?" (map->query-string params)))
                         :user (find-user email))
                app)]
    (assoc res :rows (when (= 200 (:status res))
                       (csv/read-csv (:body res))))))

(def ^:private header
  ["Date" "Description" "Amount" "Balance" "Memo" "Other Account" "Reconciled"])

(dbtest a-user-can-export-the-transactions-of-an-account
  (with-context context
    (let [{:keys [rows headers] :as res} (export "john@doe.com")]
      (is (http-success? res))
      (is (re-find #"^text/csv" (get-in headers ["Content-Type"]))
          "The response is CSV")
      (is (= "attachment; filename=\"checking-transactions.csv\""
             (get-in headers ["Content-Disposition"]))
          "The response is a file named for the account")
      (is (= [header
              ["2017-01-01" "Paycheck" "1000.00" "1000.00" "first of the year" "Salary" "yes"]
              ["2017-01-05" "Landlord" "-1000.00" "0.00" "" "Rent" "yes"]
              ["2017-01-15" "Paycheck" "800.00" "800.00" "net pay" "multiple" "no"]
              ["2017-01-20" "Kroger" "-100.25" "699.75" "" "Groceries" "no"]]
             rows)
          "Each transaction is a row, oldest first, with every field"))))

(dbtest a-user-can-export-transactions-within-a-date-range
  (with-context context
    (is (= [header
            ["2017-01-05" "Landlord" "-1000.00" "0.00" "" "Rent" "yes"]
            ["2017-01-15" "Paycheck" "800.00" "800.00" "net pay" "multiple" "no"]]
           (:rows (export "john@doe.com"
                          :start-date "2017-01-02"
                          :end-date "2017-01-15"))))))

(dbtest a-user-can-export-transactions-since-the-last-zero-balance
  (with-context context
    (is (= [header
            ["2017-01-15" "Paycheck" "800.00" "800.00" "net pay" "multiple" "no"]
            ["2017-01-20" "Kroger" "-100.25" "699.75" "" "Groceries" "no"]]
           (:rows (export "john@doe.com"
                          :since-zero-balance true
                          ; ignored in favor of the zero balance
                          :start-date "2017-01-18"))))))

(dbtest a-user-can-choose-the-fields-to-export
  (with-context context
    (is (= [["Date" "Amount" "Reconciled"]
            ["2017-01-01" "1000.00" "yes"]
            ["2017-01-05" "-1000.00" "yes"]
            ["2017-01-15" "800.00" "no"]
            ["2017-01-20" "-100.25" "no"]]
           (:rows (export "john@doe.com"
                          ; given out of order, and with an unknown field
                          :fields "reconciled,amount,bogus,transaction-date")))
        "The chosen fields are written in the usual order")))

(deftest a-user-cannot-export-the-transactions-of-an-account-in-anothers-entity
  (with-context context
    (is (http-not-found? (export "jane@doe.com")))))
