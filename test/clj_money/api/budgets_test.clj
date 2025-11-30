(ns clj-money.api.budgets-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.entities :as entities]
            [clj-money.dates :refer [periodic-seq]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-account
                                            find-budget]]
            [clj-money.util :as util :refer [make-series
                                             entity=]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private create-context
  basic-context)

(defn- create-budget
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body #:budget{:name "2020"
                               :start-date (t/local-date 2020 1 1)
                               :period [12 :month]}}}]
  (let [entity (find-entity "Personal")
        response (-> (request :post (path :api
                                          :entities
                                          (:id entity)
                                          :budgets)
                              :content-type content-type
                              :body body
                              :user (find-user email))
                     app
                     parse-body)]
    [response (-> response
                  :parsed-body
                  :id
                  entities/find)]))

(defn- assert-successful-create
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected #:budget{:name "2020"
                             :start-date (t/local-date 2020 1 1)
                             :period [12 :month]}}}]
  (is (http-created? response))
  (is (nil? (::v/errors parsed-body)) "There are no validation errors")
  (is (comparable? (or expected-response
                       expected)
                   parsed-body)
      "The response contains the newly created budget")
  (is (comparable? expected retrieved)
      "The record can be retrieved after create"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved)
      "The record is not created."))

(deftest a-user-can-create-a-budget-in-his-entity
  (with-context create-context
    (testing "default format"
      (assert-successful-create (create-budget "john@doe.com")))
    (testing "json format"
      (assert-successful-create
        (create-budget "john@doe.com"
                       :content-type "application/json"
                       :body {:name "2021"
                              :startDate "2021-01-01"
                              :period [12 "month"]
                              :_type "budget"})
        :expected #:budget{:name "2021"
                           :start-date (t/local-date 2021 1 1)
                           :period [12 :month]}
        :expected-response {:name "2021"
                            :startDate "2021-01-01"
                            :period [12 "month"]
                            :_type "budget"}))))

(deftest a-user-cannot-create-a-budget-in-anothers-entity
  (with-context create-context
    (assert-blocked-create (create-budget "jane@doe.com"))))

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

(defn- autocreate-budget-items
  [& {:keys [content-type body]
      :or {content-type "application/edn"
           body #:budget{:name "2020"
                         :start-date (t/local-date 2017 1 1)
                         :period [12 :month]
                         :auto-create-start-date (t/local-date 2016 1 1)}}}]
  (-> (request :post (path :api
                           :entities
                           (:id (find-entity "Personal"))
                           :budgets)
               :content-type content-type
               :body body
               :user (find-user "john@doe.com"))
      app
      parse-body))

(defn- assert-successful-autocreate
  [{:as response :keys [parsed-body]}]
  (let [salary (find-account "Salary")
        groceries (find-account "Groceries")
        budget (entities/find (:id parsed-body))
        items (entities/select {:budget-item/budget budget})]
    (is (http-created? response))
    (is (= 2 (count items))
        "The created budget contains an item for each income statement account with transaction items in the specified time frame")
    (is (= [500.0M 400.0M 400.0M
            500.0M 400.0M 400.0M
            500.0M 400.0M 500.0M
            400.0M 400.0M 500.0M]
           (->> items
                (filter #(entity= groceries (:budget-item/account %)))
                (map :budget-item/periods)
                first))
        "The response contains periods calculated from the transaction history for the groceries account")
    (is (= [3000.0M 2000.0M 2000.0M
            2000.0M 2000.0M 2000.0M
            3000.0M 2000.0M 2000.0M
            2000.0M 2000.0M 3000.0M]
           (->> items
                (filter #(entity= salary (:budget-item/account %)))
                (map :budget-item/periods)
                first))
        "The response contains periods calculated from the transaction history for the salary account")))

(deftest a-user-can-auto-create-items-from-history
  (with-context auto-create-context
    (testing "default format"
      (assert-successful-autocreate (autocreate-budget-items)))
    (testing "json format"
      (assert-successful-autocreate
        (autocreate-budget-items
          :content-type "application/json"
          :body {:name "2021"
                 :startDate "2018-01-01"
                 :period [12 "month"]
                 :autoCreateStartDate "2016-01-01"
                 :_type "budget"})))))

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
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (path :api
                          :entities
                          (:id (find-entity "Personal"))
                          :budgets)
               :content-type content-type
               :user (find-user email))
      app
      parse-body))

(defn- assert-successful-get-list
  [{:as response :keys [parsed-body]}
   & {:keys [expected]
      :or {expected [#:budget{:name "2016"
                              :start-date (t/local-date 2016 1 1)}
                     #:budget{:name "2015"
                              :start-date (t/local-date 2015 1 1)}]}}]
  (is (http-success? response))
  (is (seq-of-maps-like? expected parsed-body)))

(defn- assert-blocked-get-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (empty? parsed-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-budgets-for-his-entity
  (with-context list-context
    (testing "default format (edn)"
      (assert-successful-get-list (get-budgets "john@doe.com")))
    (testing "json format"
      (assert-successful-get-list
        (get-budgets "john@doe.com" :content-type "application/json")
        :expected [{:name "2016"
                    :startDate "2016-01-01"
                    :_type "budget"}
                   {:name "2015"
                    :startDate "2015-01-01"
                    :_type "budget"}]))))

(deftest a-user-cannot-get-a-list-of-budgets-for-anothers-entity
  (with-context list-context
    (assert-blocked-get-list (get-budgets "jane@doe.com"))))

(defn- get-budget
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (path :api
                          :budgets
                          (:id (find-budget "2015")))
               :content-type content-type
               :user (find-user email))
      app
      parse-body))

(defn- assert-successful-get
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (= 3 (count (:budget/items parsed-body)))
      "The items are included")
  (is (= #{{:budget-item/periods #{1000M}}
           {:budget-item/periods #{500M}}
           {:budget-item/periods #{200M}}}
         (->> (:budget/items parsed-body)
              (map #(-> %
                        (select-keys [:budget-item/periods])
                        (update-in [:budget-item/periods] set)))
              set))
      "The budget items are included in the response."))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-detailed-budget-for-his-entity
  (with-context list-context
    (testing "default format (edn)"
      (assert-successful-get (get-budget "john@doe.com")))
    (testing "json format"
      (let [{:as res :keys [parsed-body]} (get-budget "john@doe.com"
                                                      :content-type "application/json")]
        (is (http-success? res))
        (is (= 3 (count (:items parsed-body)))
            "The items are included in resposne")
        (is (= #{{:periods #{1000.0}}
                 {:periods #{500.0}}
                 {:periods #{200.0}}}
               (->> (:items parsed-body)
                    (map (fn [item]
                           (-> item
                               (select-keys [:periods])
                               (update-in [:periods] (comp set
                                                           #(map :d %))))))
                    set))
            "The budget items are included in the response.")))))

(deftest a-user-cannot-get-a-detailed-budget-for-anothers-entity
  (with-context list-context
    (assert-blocked-get (get-budget "jane@doe.com"))))

(defn- update-budget
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body {:budget/period [6 :month]}}}]
  (let [budget (find-budget "2016")
        response (-> (request :patch (path :api
                                           :budgets
                                           (:id budget))
                              :content-type content-type
                              :body body
                              :user (find-user email))
                     app
                     parse-body)]
    [response (entities/find budget)]))

(defn- assert-successful-update
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected {:budget/period [6 :month]}}}]
  (is (http-success? response))
  (is (empty? (v/error-messages parsed-body)))
  (is (comparable? (or expected-response
                       expected)
                   parsed-body)
      "The updated budget is returned")
  (is (comparable? expected retrieved)
      "The updated budget can be retrieved"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:budget/period [12 :month]}
                   retrieved)
      "The record is not updated"))

(deftest a-user-can-update-a-budget-in-his-entity
  (with-context list-context
    (testing "default format"
      (assert-successful-update (update-budget "john@doe.com")))
    (testing "json format"
      (assert-successful-update
        (update-budget "john@doe.com"
                       :content-type "application/json"
                       :body {:period [4 "month"]
                              :_type "budget"})
        :expected-response {:period [4 "month"]}
        :expected {:budget/period [4 :month]}))))

(deftest a-user-cannot-update-a-budget-in-anothers-entity
  (with-context list-context
    (assert-blocked-update (update-budget "jane@doe.com"))))

(defn- delete-budget
  [email]
  (with-context list-context
    (let [budget (find-budget "2016")
          response (-> (req/request :delete (path :api
                                                  :budgets
                                                  (:id budget)))
                       (add-auth (find-user email))
                       app)]
      [response (entities/find budget)])))

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
