(ns clj-money.api.budgets-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.test]
            [clj-money.models.ref]
            [clj-money.db.ref]
            [clj-money.models :as models]
            [clj-money.dates :refer [periodic-seq]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
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
                       (edn-body #:budget{:name "2020"
                                          :start-date (t/local-date 2020 1 1)
                                          :period [12 :month]})
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (models/select {:budget/entity entity})])))

(defn- assert-successful-create
  [[{:as response :keys [edn-body]} [retrieved]]]
  (is (http-created? response))
  (is (nil? (::v/errors edn-body)) "There are no validation errors")
  (let [expected #:budget{:name "2020"
                          :start-date (t/local-date 2020 1 1)
                          :period [12 :month]}]
    (is (comparable? expected edn-body)
        "The response contains the newly created budget")
    (is (comparable? expected retrieved)
        "The record can be retrieved after create")))

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
          {:keys [edn-body]
           :as response} (-> (req/request :post (path :api
                                                      :entities
                                                      (:id (find-entity "Personal"))
                                                      :budgets))
                             (edn-body #:budget{:name "2020"
                                                     :start-date (t/local-date 2017 1 1)
                                                     :period [12 :month]
                                                     :auto-create-start-date (t/local-date 2016 1 1)})
                             (add-auth (find-user "john@doe.com"))
                             app
                             parse-edn-body)]
      (is (http-created? response))
      (is (= 2 (count (:budget/items edn-body)))
          "The created budget contains an item for each income statement account with transaction items in the specified time frame")
      (is (= [500.0M 400.0M 400.0M
              500.0M 400.0M 400.0M
              500.0M 400.0M 500.0M
              400.0M 400.0M 500.0M]
             (->> (:budget/items edn-body)
                  (filter #(model= groceries (:budget-item/account %)))
                  (map :budget-item/periods)
                  first))
          "The response contains periods calculated from the transaction history for the groceries account")
      (is (= [3000.0M 2000.0M 2000.0M
              2000.0M 2000.0M 2000.0M
              3000.0M 2000.0M 2000.0M
              2000.0M 2000.0M 3000.0M]
             (->> (:budget/items edn-body)
                  (filter #(model= salary (:budget-item/account %)))
                  (map :budget-item/periods)
                  first))
          "The response contains periods calculated from the transaction history for the salary account"))))

(def ^:private list-context
  (conj create-context
        #:entity{:user "john@doe.com"
                 :name "Other Household"}
        #:commodity{:entity "Other Household"
                    :type :currency
                    :name "US Dollar"
                    :symbol "USD"}
        #:account{:entity "Other Household"
                  :type :income
                  :name "Salary - B"
                  :commodity "USD"}
        #:budget{:name "2015"
                 :entity "Personal"
                 :period [12 :month]
                 :start-date (t/local-date 2015 1 1)
                 :items [#:budget-item{:account "Salary"
                                       :periods (repeat 12 1000M)}
                         #:budget-item{:account "Rent"
                                       :periods (repeat 12 500M)}
                         #:budget-item{:account "Groceries"
                                       :periods (repeat 12 200M)}]}
        #:budget{:name "2015 - B"
                 :entity "Other Household"
                 :period [12 :month]
                 :start-date (t/local-date 2015 1 1)
                 :items [#:budget-item{:account "Salary - B"
                                       :periods (repeat 12 1000M)}]}
        #:budget{:name "2016"
                 :entity "Personal"
                 :period [12 :month]
                 :start-date (t/local-date 2016 1 1)}))

(defn- get-budgets
  [email]
  (with-context list-context
    (-> (req/request :get (path :api
                                :entities
                                (:id (find-entity "Personal"))
                                :budgets))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-get-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:budget{:name "2016"
                                   :start-date (t/local-date 2016 1 1)}
                          #:budget{:name "2015"
                                   :start-date (t/local-date 2015 1 1)}]
                         edn-body)))

(defn- assert-blocked-get-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-budgets-for-his-entity
  (assert-successful-get-list (get-budgets "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-budgets-for-anothers-entity
  (assert-blocked-get-list (get-budgets "jane@doe.com")))

(defn- get-budget
  [email]
  (with-context list-context
    (-> (req/request :get (path :api
                                :budgets
                                (:id (find-budget "2015"))))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= 3 (count (:budget/items edn-body)))
      "The items are included")
  (is (= #{{:budget-item/periods #{1000M}}
           {:budget-item/periods #{500M}}
           {:budget-item/periods #{200M}}}
         (->> (:budget/items edn-body)
              (map #(-> %
                        (select-keys [:budget-item/periods])
                        (update-in [:budget-item/periods] set)))
              set))
      "The budget items are included in the response."))

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
                       (edn-body (assoc budget :budget/period [6 :month]))
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (models/find budget)])))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (is (empty? (v/error-messages edn-body)))
  (is (comparable? {:budget/period [6 :month]}
                   edn-body)
      "The updated budget is returned")
  (is (comparable? {:budget/period [6 :month]}
                   retrieved)
      "The updated budget can be retrieved"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:budget/period [12 :month]}
                   retrieved)
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
