(ns clj-money.api.budgets-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.test]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-json-body]]
            [clj-money.test-context :refer [basic-context
                                            realize
                                            find-user
                                            find-entity
                                            find-account
                                            find-budget]]
            [clj-money.models.budgets :as budgets]
            [clj-money.util :refer [make-series]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private create-context
  basic-context)

(defn- create-budget
  [email]
  (let [ctx (realize create-context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :budgets))
                     (req/json-body {:name "2020"
                                     :start-date "2020-01-01"
                                     :period "month"
                                     :period-count 12})
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (budgets/search {:entity-id (:id entity)})]
    [response body retrieved]))

(defn- assert-successful-create
  [[response body [retrieved]]]
  (is (http-created? response))
  (is (nil? (::v/errors body)) "There are no validation errors")
  (is (comparable? {:name "2020"
                    :start-date "2020-01-01"
                    :period "month"
                    :period-count 12}
                   body)
      "The response contains the newly created budget")
  (is (comparable? {:name "2020"
                          :start-date (t/local-date 2020 1 1)
                          :period :month
                          :period-count 12}
                   retrieved)
       "The record can be retrieved after create"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (not ((->> retrieved (map :name) set) "2020"))
      "The record is not created."))

(deftest a-user-can-create-a-budget-in-his-entity
  (assert-successful-create (create-budget "john@doe.com")))

(deftest a-user-cannot-create-a-budget-in-anothers-entity
  (assert-blocked-create (create-budget "jane@doe.com")))

(defn- transaction-date-seq
  [start end period]
  (map (fn [d] {:transaction-date d})
       (periodic-seq start
                     end
                     period)))

(def ^:private auto-create-context
  (assoc create-context
         :transactions (concat (apply make-series {:description "Paycheck"
                                                   :entity-id "Personal"
                                                   :quantity 1000M
                                                   :debit-account-id "Checking"
                                                   :credit-account-id "Salary"}
                                      (transaction-date-seq (t/local-date 2016 1 1)
                                                            (t/local-date 2016 12 31)
                                                            (t/weeks 2)))
                               (apply make-series {:description "Kroger"
                                                   :entity-id "Personal"
                                                   :quantity 100M
                                                   :debit-account-id "Groceries"
                                                   :credit-account-id "Checking"}
                                      (transaction-date-seq (t/local-date 2016 1 1)
                                                            (t/local-date 2016 12 31)
                                                            (t/weeks 1))))))

(deftest a-user-can-auto-create-items-from-history
  (let [ctx (realize auto-create-context)
        user (find-user ctx "john@doe.com")
        entity (find-entity ctx "Personal")
        salary (find-account ctx "Salary")
        groceries (find-account ctx "Groceries")
        {:keys [json-body]
         :as response} (-> (req/request :post (path :api
                                                    :entities
                                                    (:id entity)
                                                    :budgets))
                           (req/json-body {:name "2020"
                                           :start-date "2017-01-01"
                                           :period "month"
                                           :period-count 12
                                           :auto-create-start-date "2016-01-01"})
                           (add-auth user)
                           app
                           parse-json-body)]
    (is (http-created? response))
    (is (= 2 (count (:items json-body)))
        "The created budget contains an item for each income statement account with transaction items in the specified time frame")
    (is (= [500.0 400.0 400.0
            500.0 400.0 400.0
            500.0 400.0 500.0
            400.0 400.0 500.0]
           (:periods (->> (:items json-body)
                          (filter #(= (:id groceries) (:account-id %)))
                          first))))
    (is (= [3000.0 2000.0 2000.0
            2000.0 2000.0 2000.0
            3000.0 2000.0 2000.0
            2000.0 2000.0 3000.0]
           (:periods (->> (:items json-body)
                          (filter #(= (:id salary) (:account-id %)))
                          first))))))

(def ^:private list-context
  (assoc create-context
         :budgets [{:name "2015"
                    :entity-id "Personal"
                    :period :month
                    :period-count 12
                    :start-date (t/local-date 2015 1 1)
                    :items [{:account-id "Salary"    :periods (repeat 12 1000M)}
                            {:account-id "Rent"      :periods (repeat 12 500M)}
                            {:account-id "Groceries" :periods (repeat 12 200M)}]}
                   {:name "2016"
                    :entity-id "Personal"
                    :period :month
                    :period-count 12
                    :start-date (t/local-date 2016 1 1)
                    :items [{:account-id "Salary"    :periods (repeat 12 1001M)}
                            {:account-id "Rent"      :periods (repeat 12 501M)}
                            {:account-id "Groceries" :periods (repeat 12 201M)}]}]))

(defn- get-budgets
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :get (path :api
                                             :entities
                                             (:id entity)
                                             :budgets))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-get-list
  [[response body]]
  (is (http-success? response))
  (is (= [{:name "2016"
           :start-date "2016-01-01"}
          {:name "2015"
           :start-date "2015-01-01"}]
         (map #(select-keys % [:name :start-date])
              body))))

(defn- assert-blocked-get-list
  [[response body]]
  (is (http-success? response))
  (is (empty? body) "The body is empty"))

(deftest a-user-can-get-a-list-of-budgets-for-his-entity
  (assert-successful-get-list (get-budgets "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-budgets-for-anothers-entity
  (assert-blocked-get-list (get-budgets "jane@doe.com")))

(defn- get-budget
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        budget (find-budget ctx "2016")
        response (-> (req/request :get (path :api
                                             :budgets
                                             (:id budget)))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-get
  [[response body]]
  (is (http-success? response))
  (is (= 3 (count (:items body)))
      "The items are included")
  (is (some #(= 1001.0 (first (:periods %)))
            (:items body))
      "The salary item is present in the response")
  (is (some #(= 501.0 (first (:periods %)))
            (:items body))
      "The rent item is present in the response")
  (is (some #(= 201.0 (first (:periods %)))
            (:items body))
      "The groceries item is present in the response"))

(defn- assert-blocked-get
  [[response]]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-detailed-budget-for-his-entity
  (assert-successful-get (get-budget "john@doe.com")))

(deftest a-user-cannot-get-a-detailed-budget-for-anothers-entity
  (assert-blocked-get (get-budget "jane@doe.com")))

(defn- update-budget
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        budget (find-budget ctx "2016")
        response (-> (req/request :patch (path :api
                                               :budgets
                                               (:id budget)))
                     (req/json-body (assoc-in budget [:items 1 :periods] (repeat 12 502)))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (budgets/find budget)]
    [response body retrieved]))

(defn- assert-successful-update
  [[response body retrieved]]
  (is (http-success? response))
  (is (empty? (v/error-messages body)))
  (is (= (repeat 12 502.0)
         (get-in body [:items 1 :periods]))
      "The response contains the updated budget")
  (is (= [502M 502M 502M 502M 502M 502M 502M 502M 502M 502M 502M 502M]
         (get-in retrieved [:items 1 :periods]))
      "The retrieved value contains the update attributes"))

(defn- assert-blocked-update
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:periods [501M 501M 501M 501M 501M 501M 501M 501M 501M 501M 501M 501M]}
                   (get-in retrieved [:items 1]))
      "The record is not updated"))

(deftest a-user-can-update-a-budget-in-his-entity
  (assert-successful-update (update-budget "john@doe.com")))

(deftest a-user-cannot-update-a-budget-in-anothers-entity
  (assert-blocked-update (update-budget "jane@doe.com")))

(defn- delete-budget
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        budget (find-budget ctx "2016")
        response (-> (req/request :delete (path :api
                                                :budgets
                                                (:id budget)))
                     (add-auth user)
                     app)
        retrieved (budgets/find budget)]
    [response retrieved]))

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
