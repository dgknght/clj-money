(ns clj-money.api.reconciliations-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test :refer [parse-json-body]]
            [dgknght.app-lib.web :refer [path]]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-transaction-item
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
                         :item-refs [[(t/local-date 2015 1 1) 1000M]
                                     [(t/local-date 2015 1 2) 500M]
                                     [(t/local-date 2015 1 3) 100M]]}))

(defn- get-reconciliations
  [email]
  (with-context recon-context
    (let [user (find-user email)
          account (find-account "Checking")]
      (-> (req/request :get (path :api
                                  :accounts
                                  (:id account)
                                  :reconciliations))
          (add-auth user)
          app
          parse-json-body))))

(defn- assert-successful-get
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:reconciliation{:end-of-period "2015-01-04"
                                           :balance 400.0}]
                         json-body)
      "The response contains the list of reconciliations"))

(defn- assert-blocked-get
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (empty? json-body) "No reconciliations are returned"))

(deftest a-user-can-get-a-reconciliations-from-his-entity
  (assert-successful-get (get-reconciliations "john@doe.com")))

(deftest a-user-cannot-get-a-reconciliations-from-anothers-entity
  (assert-blocked-get (get-reconciliations "jane@doe.com")))

(defn- create-reconciliation
  [email complete?]
  (with-context recon-context
    (let [user (find-user email)
          account (find-account "Checking")
          item-refs (if complete?
                      (let [item (find-transaction-item [(t/local-date 2015 1 10)
                                                         101M
                                                         account])]
                        [((juxt :id :transaction-item/transaction-date) item)])
                      [])
          status (if complete?
                   :completed
                   :new)
          response (-> (req/request :post (path :api
                                                :accounts
                                                (:id account)
                                                :reconciliations))
                       (req/json-body #:reconciliation{:end-of-period "2015-02-04"
                                                       :balance 299.0
                                                       :status status
                                                       :item-refs item-refs})
                       (add-auth user)
                       app
                       parse-json-body)
          retrieved (models/find-by
                      #:reconciliation{:account account
                                       :end-of-period (t/local-date 2015 2 4)})]
      [response retrieved])))

(defn- assert-create-succeeded
  [[{:as response :keys [json-body]} retrieved]]
  (is (http-created? response))
  (is (valid? json-body))
  (is (comparable? #:reconciliation{:end-of-period "2015-02-04"
                                    :balance 299.0}
                   json-body)
      "The body contains the created reconciliation")
  (is (comparable? #:reconciliation{:end-of-period (t/local-date 2015 2 4)
                                    :balance 299M}
                   retrieved)
      "The newly created reconciliation can be retrieved"))

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

; (def ^:private update-context
;   (update-in recon-context
;              [:reconciliations]
;              conj
;              {:end-of-period (t/local-date 2015 2 4)
;               :account-id "Checking"
;               :balance 299M
;               :status :new
;               :item-refs [{:transaction-date (t/local-date 2015 1 10)
;                            :quantity 101M}]}))
; 
; (defn- update-reconciliation
;   [email]
;   (let [ctx (realize update-context)
;         recon (find-reconciliation ctx "Checking" (t/local-date 2015 2 4))
;         user (find-user ctx email)
;         response (-> (req/request :patch (path :api
;                                                :reconciliations
;                                                (:id recon)))
;                      (req/json-body (-> recon
;                                         (dissoc :id)
;                                         (assoc :status :completed)))
;                      (add-auth user)
;                      app)
;         body (json/parse-string (:body response) true)
;         retrieved (recs/find recon)]
;     [response body retrieved]))
; 
; (defn- assert-successful-update
;   [[response body retrieved]]
;   (is (http-success? response))
;   (is (= "completed" (:status body))
;       "The response includes the updated reconciliation")
;   (is (= :completed (:status retrieved))
;       "The reconciliation is updated in the database"))
; 
; (defn- assert-blocked-update
;   [[response _ retrieved]]
;   (is (http-not-found? response))
;   (is (= :new (:status retrieved))
;       "The reconciliation is not updated in the database"))
; 
; (deftest a-user-can-update-a-reconciliation-in-his-entity
;   (assert-successful-update (update-reconciliation "john@doe.com")))
; 
; (deftest a-user-cannot-update-a-reconciliation-in-anothers-entity
;   (assert-blocked-update (update-reconciliation "jane@doe.com")))
