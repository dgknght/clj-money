(ns clj-money.api.reconciliations-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [ring.mock.request :as req]
            [clj-time.core :as t]
            [cheshire.core :as json]
            [clj-money.validation :as v]
            [clj-money.util :refer [path]]
            [clj-money.test-helpers :refer [reset-db
                                            selective=]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-successful-create
                                                assert-not-found]]
            [clj-money.test-context :refer [basic-context
                                            realize
                                            find-user
                                            find-account
                                            find-transaction-item
                                            find-recon]]
            [clj-money.web.server :refer [app]]
            [clj-money.models.reconciliations :as recs]))

(use-fixtures :each reset-db)

(def ^:private recon-context
  (assoc basic-context
         :transactions [{:entity-id "Personal"
                         :description "Paycheck"
                         :transaction-date (t/local-date 2015 1 1)
                         :debit-account-id "Checking"
                         :credit-account-id "Salary"
                         :quantity 1000M}
                        {:entity-id "Personal"
                         :description "Landlord"
                         :transaction-date (t/local-date 2015 1 2)
                         :debit-account-id "Rent"
                         :credit-account-id "Checking"
                         :quantity 500M}
                        {:entity-id "Personal"
                         :description "Kroger"
                         :transaction-date (t/local-date 2015 1 3)
                         :debit-account-id "Groceries"
                         :credit-account-id "Checking"
                         :quantity 100M}
                        {:entity-id "Personal"
                         :description "Kroger"
                         :transaction-date (t/local-date 2015 1 10)
                         :debit-account-id "Groceries"
                         :credit-account-id "Checking"
                         :quantity 101M}]
         :reconciliations [{:account-id "Checking"
                            :balance 400M
                            :status :completed
                            :end-of-period (t/local-date 2015 1 4)
                            :item-refs [{:transaction-date (t/local-date 2015 1 1)
                                         :quantity 1000M}
                                        {:transaction-date (t/local-date 2015 1 2)
                                         :quantity 500M}
                                        {:transaction-date (t/local-date 2015 1 3)
                                         :quantity 100M}]}]))

(defn- get-reconciliations
  [email]
  (let [ctx (realize recon-context)
        user (find-user ctx email)
        account (find-account ctx "Checking")
        response (-> (req/request :get (path :api
                                             :accounts
                                             (:id account)
                                             :reconciliations))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-get
  [[response body]]
  (assert-successful response)
  (is (= 1 (count body))
      "The correct number of reconciliations is returned")
  (is (selective= {:end-of-period "2015-01-04"
                   :balance 400.0}
                  (first body))
      "The correct reconciliation records are returned"))

(defn- assert-blocked-get
  [[response body]]
  (assert-successful response)
  (is (empty? body) "No reconciliations are returned"))

(deftest a-user-can-get-a-reconciliations-from-his-entity
  (assert-successful-get (get-reconciliations "john@doe.com")))

(deftest a-user-cannot-get-a-reconciliations-from-anothers-entity
  (assert-blocked-get (get-reconciliations "jane@doe.com")))

(defn- create-reconciliation
  [email complete?]
  (let [ctx (realize recon-context)
        user (find-user ctx email)
        account (find-account ctx "Checking")
        item-refs (if complete?
                    (let [item (find-transaction-item ctx (t/local-date 2015 1 10)
                                                      101M
                                                      (:id account))]
                      [((juxt :id :transaction-date) item)])
                    [])
        status (if complete?
                 :completed
                 :new)
        response (-> (req/request :post (path :api
                                              :accounts
                                              (:id account)
                                              :reconciliations))
                     (req/json-body {:end-of-period "2015-02-04"
                                     :balance 299.0
                                     :status status
                                     :item-refs item-refs})
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (recs/search {:account-id (:id account)})]
    [response body retrieved]))

(defn- assert-create-succeeded
  [[response body retrieved]]
  (assert-successful-create response)
  (is (nil? (::v/errors body))
      "There are no validation errors")
  (is (selective= {:end-of-period "2015-02-04"
                   :balance 299.0}
                  body)
      "The body contains the created reconciliation")
  (is (some #(selective= {:end-of-period (t/local-date 2015 2 4)
                          :balance 299M}
                         %)
            retrieved)
      "The newly created reconciliation can be retrieved"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (assert-not-found response)
  (is (not-any? #(selective= {:end-of-period (t/local-date 2015 2 4)
                              :balance 300M}
                             %)
                retrieved)
      "The reconciliation is not created"))

(deftest a-user-can-create-a-completed-reconciliation-in-his-entity
  (assert-create-succeeded (create-reconciliation "john@doe.com" true)))

(deftest a-user-can-create-an-incomplete-reconciliation-in-his-entity
  (assert-create-succeeded (create-reconciliation "john@doe.com" false)))

(deftest a-user-cannot-create-a-completed-reconciliation-in-anothers-entity
  (assert-blocked-create (create-reconciliation "jane@doe.com" true)))

(deftest a-user-cannot-create-an-incomplete-reconciliation-in-anothers-entity
  (assert-blocked-create (create-reconciliation "jane@doe.com" false)))

(def ^:private update-context
  (update-in recon-context
             [:reconciliations]
             conj
             {:end-of-period (t/local-date 2015 2 4)
              :account-id "Checking"
              :balance 299M
              :status :new
              :item-refs [{:transaction-date (t/local-date 2015 1 10)
                           :quantity 101M}]}))

(defn- update-reconciliation
  [email]
  (let [ctx (realize update-context)
        recon (find-recon ctx "Checking" (t/local-date 2015 2 4))
        user (find-user ctx email)
        response (-> (req/request :patch (path :api
                                               :reconciliations
                                               (:id recon)))
                     (req/json-body (-> recon
                                        (dissoc :id)
                                        (assoc :status :completed)))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (recs/find recon)]
    [response body retrieved]))

(defn- assert-successful-update
  [[response body retrieved]]
  (assert-successful response)
  (is (= "completed" (:status body))
      "The response includes the updated reconciliation")
  (is (= :completed (:status retrieved))
      "The reconciliation is updated in the database"))

(defn- assert-blocked-update
  [[response _ retrieved]]
  (assert-not-found response)
  (is (= :new (:status retrieved))
      "The reconciliation is not updated in the database"))

(deftest a-user-can-update-a-reconciliation-in-his-entity
  (assert-successful-update (update-reconciliation "john@doe.com")))

(deftest a-user-cannot-update-a-reconciliation-in-anothers-entity
  (assert-blocked-update (update-reconciliation "jane@doe.com")))
