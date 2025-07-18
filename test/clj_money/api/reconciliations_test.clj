(ns clj-money.api.reconciliations-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.web :refer [path]]
            [clj-money.models.ref]
            [clj-money.util :as util]
            [clj-money.db.sql.ref]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-reconciliation]]
            [clj-money.web.server :refer [app]]
            [clj-money.models :as models]))

(use-fixtures :each reset-db)

(def ^:private recon-context
  (conj basic-context
        #:transaction{:entity "Personal"
                      :description "Paycheck"
                      :transaction-date (t/local-date 2015 1 1)
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:entity "Personal"
                      :description "Landlord"
                      :transaction-date (t/local-date 2015 1 2)
                      :debit-account "Rent"
                      :credit-account "Checking"
                      :quantity 500M}
        #:transaction{:entity "Personal"
                      :description "Kroger"
                      :transaction-date (t/local-date 2015 1 3)
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 100M}
        #:transaction{:entity "Personal"
                      :description "Kroger"
                      :transaction-date (t/local-date 2015 1 10)
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 101M}
        #:reconciliation{:account "Checking"
                         :balance 400M
                         :status :completed
                         :end-of-period (t/local-date 2015 1 4)
                         :items [[(t/local-date 2015 1 1) 1000M]
                                 [(t/local-date 2015 1 2) 500M]
                                 [(t/local-date 2015 1 3) 100M]]}))

(defn- get-reconciliations
  [email]
  (with-context recon-context
    (-> (req/request :get (path :api
                                :accounts
                                (:id (find-account "Checking"))
                                :reconciliations))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:reconciliation{:end-of-period (t/local-date 2015 1 4)
                                           :balance 400M}]
                         edn-body)
      "The response contains the list of reconciliations"))

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
  (with-context recon-context
    (let [user (find-user email)
          account (find-account "Checking")
          items (if complete?
                  (map #(select-keys % [:id :transaction/transaction-date])
                       (models/select {:transaction-item/account account
                                       :transaction-item/quantity 101M
                                       :transaction/transaction-date (t/local-date 2015 1 10)}
                                      {:select-also [:transaction/transaction-date]}))
                  [])
          status (if complete?
                   :completed
                   :new)
          response (-> (req/request :post (path :api
                                                :accounts
                                                (:id account)
                                                :reconciliations))
                       (edn-body #:reconciliation{:end-of-period (t/local-date 2015 2 4)
                                                  :balance 299.0M
                                                  :status status
                                                  :items items})
                       (add-auth user)
                       app
                       parse-edn-body)
          retrieved (models/find-by
                      #:reconciliation{:account account
                                       :end-of-period (t/local-date 2015 2 4)})]
      [response retrieved])))

(defn- assert-create-succeeded
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-created? response))
  (is (valid? edn-body))
  (let [expected #:reconciliation{:end-of-period (t/local-date 2015 2 4)
                                  :balance 299M}]
    (is (comparable? expected edn-body)
        "The body contains the created reconciliation")
    (is (comparable? expected retrieved)
        "The newly created reconciliation can be retrieved")))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved) "The reconciliation is not created"))

(deftest a-user-can-create-a-completed-reconciliation-in-his-entity
  (assert-create-succeeded (create-reconciliation "john@doe.com" true)))

(deftest a-user-can-create-an-incomplete-reconciliation-in-his-entity
  (assert-create-succeeded (create-reconciliation "john@doe.com" false)))

(deftest a-user-cannot-create-a-completed-reconciliation-in-anothers-entity
  (assert-blocked-create (create-reconciliation "jane@doe.com" true)))

(deftest a-user-cannot-create-an-incomplete-reconciliation-in-anothers-entity
  (assert-blocked-create (create-reconciliation "jane@doe.com" false)))

(def ^:private update-context
  (conj recon-context
        #:reconciliation{:end-of-period (t/local-date 2015 2 4)
                         :account "Checking"
                         :balance 299M
                         :status :new
                         :items [[(t/local-date 2015 1 10) 101M]]}))

(defn- update-reconciliation
  [email]
  (with-context update-context
    (let [recon (find-reconciliation ["Checking" (t/local-date 2015 2 4)])
          response (-> (req/request :patch (path :api
                                                 :reconciliations
                                                 (:id recon)))
                       (edn-body (-> recon
                                     (dissoc :id :reconciliation/items)
                                     (assoc :reconciliation/status :completed)))
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (models/find-by
                  (util/model-type
                    (select-keys recon
                                 [:id
                                  :reconciliation/end-of-period])
                    :reconciliation))])))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected {:reconciliation/status :completed}]
    (is (comparable? expected edn-body)
        "The response includes the updated reconciliation")
    (is (comparable? expected retrieved)
        "The reconciliation is updated in the database")))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:reconciliation/status :new}
                   retrieved)
      "The reconciliation is not updated in the database"))

(deftest a-user-can-update-a-reconciliation-in-his-entity
  (assert-successful-update (update-reconciliation "john@doe.com")))

(deftest a-user-cannot-update-a-reconciliation-in-anothers-entity
  (assert-blocked-update (update-reconciliation "jane@doe.com")))
