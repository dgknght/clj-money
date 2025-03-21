(ns clj-money.api.reconciliations-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [dgknght.app-lib.web :refer [path]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
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
        account (find-account ctx "Checking")]
    (-> (req/request :get (path :api
                                :accounts
                                (:id account)
                                :reconciliations))
        (add-auth user)
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:end-of-period (t/local-date 2015 1 4)
                           :balance 400.0M}]
                         edn-body)
      "The reconciliation records are returned"))

(defn- assert-blocked-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "No reconciliations are returned"))

(deftest a-user-can-get-reconciliations-from-his-entity
  (assert-successful-get (get-reconciliations "john@doe.com")))

(deftest a-user-cannot-get-reconciliations-from-anothers-entity
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
                     (edn-body {:end-of-period (t/local-date 2015 2 4)
                                :balance 299.0M
                                :status status
                                :item-refs item-refs})
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (recs/find-by {:account-id (:id account)
                                 :end-of-period (t/local-date 2015 2 4)})]
    [response retrieved]))

(defn- assert-create-succeeded
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-created? response))
  (is (valid? edn-body))
  (let [expected {:end-of-period (t/local-date 2015 2 4)
                  :balance 299M}]
    (is (comparable? expected edn-body)
        "The body contains the created reconciliation")
    (is (comparable? expected retrieved)
        "The newly created reconciliation can be retrieved")))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (nil?  retrieved) "The reconciliation is not created"))

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
                     (edn-body (-> recon
                                   (dissoc :id)
                                   (assoc :status :completed)))
                     (add-auth user)
                     app
                     parse-edn-body)]
    [response (recs/find recon)]))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected {:status :completed}]
    (is (comparable? expected edn-body)
        "The response includes the updated reconciliation")
    (is (comparable? expected retrieved)
        "The reconciliation is updated in the database")))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (= :new (:status retrieved))
      "The reconciliation is not updated in the database"))

(deftest a-user-can-update-a-reconciliation-in-his-entity
  (assert-successful-update (update-reconciliation "john@doe.com")))

(deftest a-user-cannot-update-a-reconciliation-in-anothers-entity
  (assert-blocked-update (update-reconciliation "jane@doe.com")))
