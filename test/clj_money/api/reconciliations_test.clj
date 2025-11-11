(ns clj-money.api.reconciliations-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [dgknght.app-lib.web :refer [path]]
            [clj-money.json]
            [clj-money.entities.ref]
            [clj-money.util :as util]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-reconciliation]]
            [clj-money.web.server :refer [app]]
            [clj-money.entities :as entities]))

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
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (path :api
                          :accounts
                          (:id (find-account "Checking"))
                          :reconciliations)
               :content-type content-type
               :user (find-user email))
      app
      parse-body))

(defn- assert-successful-get
  [{:as response :keys [edn-body parsed-body]}
   & {:keys [expected]
      :or {expected [#:reconciliation{:end-of-period (t/local-date 2015 1 4)
                                      :balance 400M}]}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (seq-of-maps-like? expected body)
        "The response contains the list of reconciliations")))

(defn- assert-blocked-get
  [{:as response :keys [edn-body parsed-body]}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (empty? body) "No reconciliations are returned")))

(deftest a-user-can-get-reconciliations-from-his-entity
  (with-context recon-context
    (testing "default format"
      (assert-successful-get (get-reconciliations "john@doe.com")))
    (testing "json format"
      (assert-successful-get
        (get-reconciliations "john@doe.com" :content-type "application/json")
        :expected [{:endOfPeriod "2015-01-04"
                    :balance {:d 400.0}
                    :_type "reconciliation"}]))))

(deftest a-user-cannot-get-reconciliations-from-anothers-entity
  (with-context recon-context
    (assert-blocked-get (get-reconciliations "jane@doe.com"))))

(defn- create-reconciliation
  [email & {:keys [content-type body]
            :or {content-type "application/edn"}}]
  (let [account (find-account "Checking")
        user (find-user email)
        response (-> (request :post (path :api
                                          :accounts
                                          (:id account)
                                          :reconciliations)
                              :content-type content-type
                              :body body
                              :user user)
                     app
                     parse-body)
        retrieved (entities/find-by
                    #:reconciliation{:account account
                                     :end-of-period (t/local-date 2015 2 4)})]
    [response retrieved]))

(defn- assert-create-succeeded
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected #:reconciliation{:end-of-period (t/local-date 2015 2 4)
                                     :balance 299M}}}]
  (is (http-created? response))
  (is (valid? parsed-body))
  (is (comparable? (or expected-response expected)
                   parsed-body)
      "The body contains the created reconciliation")
  (is (comparable? expected retrieved)
      "The newly created reconciliation can be retrieved"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved) "The reconciliation is not created"))

(defn- select-recon-items []
  (map #(select-keys % [:id :transaction/transaction-date])
       (entities/select {:transaction-item/account (find-account "Checking")
                         :transaction-item/quantity 101M
                         :transaction/transaction-date (t/local-date 2015 1 10)}
                        {:select-also [:transaction/transaction-date]})))

(defn- build-recon
  [status]
  (cond-> #:reconciliation{:end-of-period (t/local-date 2015 2 4)
                           :balance 299.0M
                           :status status}
    (= :completed status)
    (assoc :reconciliation/items (select-recon-items))))

(defn- build-json-recon
  [status]
  (cond-> {:endOfPeriod (t/local-date 2015 2 4)
           :balance 299.0M
           :status status
           :_type "reconciliation"}
    (= :completed status)
    (assoc :items (map (comp #(rename-keys % {:transaction/transaction-date
                                              :transactionDate})
                             #(assoc % :_type "transaction"))
                       (select-recon-items)))))

(deftest a-user-can-create-a-completed-reconciliation-in-his-entity
  (with-context recon-context
    (assert-create-succeeded
      (create-reconciliation "john@doe.com"
                             :body (build-recon :completed)))))

(deftest a-user-can-create-a-completed-reconciliation-in-his-entity-with-json
  (with-context recon-context
    (assert-create-succeeded
      (create-reconciliation "john@doe.com"
                             :content-type "application/json"
                             :body (build-json-recon :completed))
      :expected-response {:endOfPeriod "2015-02-04"
                          :balance {:d 299.0}
                          :_type "reconciliation"})))

(deftest a-user-can-create-an-incomplete-reconciliation-in-his-entity
  (with-context recon-context
    (assert-create-succeeded
        (create-reconciliation "john@doe.com"
                               :body (build-recon :new)))))

(deftest a-user-can-create-an-incomplete-reconciliation-in-his-entity-with-json
  (with-context recon-context
    (assert-create-succeeded
      (create-reconciliation "john@doe.com"
                             :content-type "application/json"
                             :body (build-json-recon :new))
      :expected-response {:endOfPeriod "2015-02-04"
                          :balance {:d 299.0}
                          :_type "reconciliation"})))

(deftest a-user-cannot-create-a-completed-reconciliation-in-anothers-entity
  (with-context recon-context
    (assert-blocked-create
      (create-reconciliation "jane@doe.com"
                             :body (build-recon :completed)))))

(deftest a-user-cannot-create-an-incomplete-reconciliation-in-anothers-entity
  (with-context recon-context
    (assert-blocked-create
      (create-reconciliation "jane@doe.com"
                             :body (build-recon :new)))))

(def ^:private update-context
  (conj recon-context
        #:reconciliation{:end-of-period (t/local-date 2015 2 4)
                         :account "Checking"
                         :balance 299M
                         :status :new
                         :items [[(t/local-date 2015 1 10) 101M]]}))

(defn- update-reconciliation
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [recon (find-reconciliation ["Checking" (t/local-date 2015 2 4)])
        body (if (= content-type "application/json")
               {:status "completed"
                :_type "reconciliation"}
               (-> recon
                   (dissoc :id :reconciliation/items)
                   (assoc :reconciliation/status :completed)))
        response (-> (request :patch (path :api
                                           :reconciliations
                                           (:id recon))
                              :content-type content-type
                              :body body
                              :user (find-user email))
                     app
                     parse-body)]
    [response (entities/find-by
                (util/entity-type
                  (select-keys recon
                               [:id
                                :reconciliation/end-of-period])
                  :reconciliation))]))

(defn- assert-successful-update
  [[{:as response :keys [edn-body parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected {:reconciliation/status :completed}}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (comparable? (or expected-response expected) body)
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
  (with-context update-context
    (assert-successful-update (update-reconciliation "john@doe.com"))))

(deftest a-user-can-update-a-reconciliation-in-his-entity-with-json
  (with-context update-context
    (assert-successful-update
      (update-reconciliation "john@doe.com" :content-type "application/json")
      :expected-response {:status "completed"
                          :_type "reconciliation"})))

(deftest a-user-cannot-update-a-reconciliation-in-anothers-entity
  (with-context update-context
    (assert-blocked-update (update-reconciliation "jane@doe.com"))))
