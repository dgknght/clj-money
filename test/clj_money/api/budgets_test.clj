(ns clj-money.api.budgets-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.test]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.models :as models]
            [clj-money.dates :refer [periodic-seq]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-json-body]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-account
                                            find-budget]]
            [clj-money.util :refer [make-series
                                    model=]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private create-context
  basic-context)

(defn- create-budget
  [email]
  (with-context create-context
    (let [entity (find-entity "Personal")
          response (-> (req/request :post (path :api
                                                :entities
                                                (:id entity)
                                                :budgets))
                       (req/json-body #:budget{:name "2020"
                                               :start-date "2020-01-01"
                                               :period "month"
                                               :period-count 12})
                       (add-auth (find-user email))
                       app
                       parse-json-body)]
      [response (models/select {:budget/entity entity})])))

(defn- assert-successful-create
  [[{:as response :keys [json-body]} [retrieved]]]
  (is (http-created? response))
  (is (nil? (::v/errors json-body)) "There are no validation errors")
  (is (comparable? #:budget{:name "2020"
                            :start-date "2020-01-01"
                            :period "month"
                            :period-count 12}
                   json-body)
      "The response contains the newly created budget")
  (is (comparable? #:budget{:name "2020"
                            :start-date (t/local-date 2020 1 1)
                            :period :month
                            :period-count 12}
                   retrieved)
      "The record can be retrieved after create"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved)
      "The record is not created."))

(deftest a-user-can-create-a-budget-in-his-entity
  (assert-successful-create (create-budget "john@doe.com")))

(deftest a-user-cannot-create-a-budget-in-anothers-entity
  (assert-blocked-create (create-budget "jane@doe.com")))

(defn- transaction-date-seq
  [start end period]
  (map #(hash-map :transaction/transaction-date %)
       (periodic-seq start
                     end
                     period)))

(def ^:private auto-create-context
  (concat create-context
          (apply make-series #:transaction{:description "Paycheck"
                                           :entity "Personal"
                                           :quantity 1000M
                                           :debit-account "Checking"
                                           :credit-account "Salary"}
                 (transaction-date-seq (t/local-date 2016 1 1)
                                       (t/local-date 2016 12 31)
                                       (t/weeks 2)))
          (apply make-series #:transaction{:description "Kroger"
                                           :entity "Personal"
                                           :quantity 100M
                                           :debit-account "Groceries"
                                           :credit-account "Checking"}
                 (transaction-date-seq (t/local-date 2016 1 1)
                                       (t/local-date 2016 12 31)
                                       (t/weeks 1)))))

(deftest a-user-can-auto-create-items-from-history
  (with-context auto-create-context
    (let [salary (find-account "Salary")
          groceries (find-account "Groceries")
          {:keys [json-body]
           :as response} (-> (req/request :post (path :api
                                                      :entities
                                                      (:id (find-entity "Personal"))
                                                      :budgets))
                             (req/json-body #:budget{:name "2020"
                                                     :start-date "2017-01-01"
                                                     :period "month"
                                                     :period-count 12
                                                     :auto-create-start-date "2016-01-01"})
                             (add-auth (find-user "john@doe.com"))
                             app
                             parse-json-body)]
      (is (http-created? response))
      (is (= 2 (count (:budget/items json-body)))
          "The created budget contains an item for each income statement account with transaction items in the specified time frame")
      (is (= [500.0 400.0 400.0
              500.0 400.0 400.0
              500.0 400.0 500.0
              400.0 400.0 500.0]
             (->> (:budget/items json-body)
                  (filter #(model= groceries (:budget-item/account %)))
                  (map :budget-item/periods)
                  first))
          "The response contains periods calculated from the transaction history for the groceries account")
      (is (= [3000.0 2000.0 2000.0
              2000.0 2000.0 2000.0
              3000.0 2000.0 2000.0
              2000.0 2000.0 3000.0]
             (->> (:budget/items json-body)
                  (filter #(model= salary (:budget-item/account %)))
                  (map :budget-item/periods)
                  first))
          "The response contains periods calculated from the transaction history for the salary account"))))

(def ^:private list-context
  (conj create-context
        #:budget{:name "2015"
                 :entity "Personal"
                 :period :month
                 :period-count 12
                 :start-date (t/local-date 2015 1 1)
                 :items [#:budget-item{:account "Salary"    :periods (vec (repeat 12 1000M))}
                         #:budget-item{:account "Rent"      :periods (vec (repeat 12 500M))}
                         #:budget-item{:account "Groceries" :periods (vec (repeat 12 200M))}]}
        #:budget{:name "2016"
                 :entity "Personal"
                 :period :month
                 :period-count 12
                 :start-date (t/local-date 2016 1 1)
                 :items [#:budget-item{:account "Salary"    :periods (vec (repeat 12 1001M))}
                         #:budget-item{:account "Rent"      :periods (vec (repeat 12 501M))}
                         #:budget-item{:account "Groceries" :periods (vec (repeat 12 201M))}]}))

(defn- get-budgets
  [email]
  (with-context list-context
    (-> (req/request :get (path :api
                                :entities
                                (:id (find-entity "Personal"))
                                :budgets))
        (add-auth (find-user email))
        app
        parse-json-body)))

(defn- assert-successful-get-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:budget{:name "2016"
                                   :start-date "2016-01-01"}
                          #:budget{:name "2015"
                                   :start-date "2015-01-01"}]
                         json-body)))

(defn- assert-blocked-get-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (empty? json-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-budgets-for-his-entity
  (assert-successful-get-list (get-budgets "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-budgets-for-anothers-entity
  (assert-blocked-get-list (get-budgets "jane@doe.com")))

(defn- get-budget
  [email]
  (with-context list-context
    (-> (req/request :get (path :api
                                :budgets
                                (:id (find-budget "2016"))))
        (add-auth (find-user email))
        app
        parse-json-body)))

(defn- assert-successful-get
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (= 3 (count (:budget/items json-body)))
      "The items are included")
  (is (some #(= 1001.0 (first (:budget-item/periods %)))
            (:budget/items json-body))
      "The salary item is present in the response")
  (is (some #(= 501.0 (first (:budget-item/periods %)))
            (:budget/items json-body))
      "The rent item is present in the response")
  (is (some #(= 201.0 (first (:budget-item/periods %)))
            (:budget/items json-body))
      "The groceries item is present in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-detailed-budget-for-his-entity
  (assert-successful-get (get-budget "john@doe.com")))

(deftest a-user-cannot-get-a-detailed-budget-for-anothers-entity
  (assert-blocked-get (get-budget "jane@doe.com")))

(defn- update-budget
  [email]
  (with-context list-context
    (let [budget (find-budget "2016")
          response (-> (req/request :patch (path :api
                                                 :budgets
                                                 (:id budget)))
                       (req/json-body (assoc-in budget
                                                [:budget/items
                                                 1
                                                 :budget-item/periods]
                                                (repeat 12 502.1M)))
                       (add-auth (find-user email))
                       app
                       parse-json-body)]
      [response (models/find budget)])))

(defn- assert-successful-update
  [[{:as response :keys [json-body]} retrieved]]
  (is (http-success? response))
  (is (empty? (v/error-messages json-body)))
  (is (= (repeat 12 502.1)
         (get-in json-body [:budget/items 1 :budget-item/periods]))
      "The response contains the updated budget")
  (is (= (repeat 12 502.1M)
         (get-in retrieved [:budget/items 1 :budget-item/periods]))
      "The retrieved value contains the update attributes"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:budget-item/periods [501M 501M 501M 501M 501M 501M 501M 501M 501M 501M 501M 501M]}
                   (get-in retrieved [:budget/items 1]))
      "The record is not updated"))

(deftest a-user-can-update-a-budget-in-his-entity
  (assert-successful-update (update-budget "john@doe.com")))

(deftest a-user-cannot-update-a-budget-in-anothers-entity
  (assert-blocked-update (update-budget "jane@doe.com")))

(defn- delete-budget
  [email]
  (with-context list-context
    (let [budget (find-budget "2016")
          response (-> (req/request :delete (path :api
                                                  :budgets
                                                  (:id budget)))
                       (add-auth (find-user email))
                       app)]
      [response (models/find budget)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-no-content? response))
  (is (nil? retrieved)
      "The delete budget cannot be retrieved"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The record is not deleted"))

(deftest a-user-can-delete-a-budget-in-his-entity
  (assert-successful-delete (delete-budget "john@doe.com")))

(deftest a-user-cannot-delete-a-budget-in-anothers-entity
  (assert-blocked-delete (delete-budget "jane@doe.com")))
