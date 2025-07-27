(ns clj-money.api.attachments-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.dates :as dates]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-transaction
                                            find-attachment]]
            [clj-money.models :as models]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private att-context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2015 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}))

(defn- create-attachment
  [email]
  (with-context att-context
    (let [transaction (find-transaction [(t/local-date 2015 1 1) "Paycheck"])
          file (io/file (io/resource "fixtures/attachment.jpg"))
          response (-> (req/request :post (path :api
                                                :transactions
                                                (:id transaction)
                                                (dates/serialize-local-date
                                                  (:transaction/transaction-date transaction))
                                                :attachments))
                       (merge (build-multipart-request {:file {:file file
                                                               :content-type "image/jpg"}
                                                        :attachment/caption "receipt"}))
                       (add-auth (find-user email))
                       (req/header "Accept" "application/edn")
                       app
                       parse-edn-body)]
      [response
       (when-let [id (get-in response [:edn-body :id])]
         (models/find id :attachment))])))

(defn- assert-successful-create
  [[{:keys [edn-body] :as response} retrieved]]
  (is (http-created? response))
  (is (empty? (::v/errors edn-body))
      "There are no validation errors")
  (is (:id edn-body) "An ID is assigned to the new record")
  (is (comparable? {:attachment/caption "receipt"}
                   retrieved) 
      "The created attachment can be retrieved"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved)))

(deftest a-user-can-create-an-attachment-in-his-entity
  (assert-successful-create (create-attachment "john@doe.com")))

(deftest a-user-cannot-create-an-attachment-in-anothers-entity
  (assert-blocked-create (create-attachment "jane@doe.com")))

(def ^:private list-context
  (conj att-context
        #:image{:user "john@doe.com"
                :original-filename "receipt.jpg"
                :content-type "image/jpg"
                :content (io/file (io/resource "fixtures/attachment.jpg"))}
        #:attachment{:caption "Receipt"
                     :transaction [(t/local-date 2015 1 1)
                                   "Paycheck"]
                     :image "receipt.jpg"}))

(defn- list-trx-attachments
  [email]
  (with-context list-context
    (let [transaction (find-transaction [(t/local-date 2015 1 1)
                                         "Paycheck"])]
      (-> (req/request :get (path :api
                                  :transactions
                                  (:id transaction)
                                  (dates/serialize-local-date
                                    (:transaction/transaction-date transaction))
                                  :attachments))
          (add-auth (find-user email))
          app
          parse-edn-body))))

(defn- list-account-attachments
  [email]
  (with-context list-context
    (let [account (find-account "Checking")]
      (-> (req/request :get (path :api
                                  :accounts
                                  (:id account)
                                  :attachments
                                  "2015-01-01"
                                  "2015-02-01"))
          (add-auth (find-user email))
          app
          parse-edn-body))))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:attachment/caption "Receipt"}]
                         edn-body)
      "The list of attachments is returned."))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "No records are returned"))

(deftest a-user-can-get-a-list-of-attachments-for-a-trx-in-his-entity
  (assert-successful-list (list-trx-attachments "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-attachments-for-a-trx-in-anothers-entity
  (assert-blocked-list (list-trx-attachments "jane@doe.com")))

(deftest a-user-can-get-a-list-of-attachments-for-an-account-in-his-entity
  (assert-successful-list (list-account-attachments "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-attachments-for-an-account-in-anothers-entity
  (assert-blocked-list (list-account-attachments "jane@doe.com")))

(defn- update-attachment
  [email]
  (with-context list-context
    (let [attachment (find-attachment "Receipt")
          response (-> (req/request :patch (path :api
                                                 :attachments
                                                 (:id attachment)))
                       (edn-body (assoc attachment
                                        :attachment/caption "Updated caption"))
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (models/find attachment)])))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (is (comparable? {:attachment/caption "Updated caption"}
                   edn-body)
      "The updated attachment is returned")
  (is (comparable? {:attachment/caption "Updated caption"}
                   retrieved)
      "The database is updated"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:attachment/caption "Receipt"}
                   retrieved)
      "The retrieved attachment has the original values"))

(deftest a-user-can-update-an-attachment-in-his-entity
  (assert-successful-update (update-attachment "john@doe.com")))

(deftest a-user-cannot-update-an-attachment-in-anothers-entity
  (assert-blocked-update (update-attachment "jane@doe.com")))

(defn- delete-attachment
  [email]
  (with-context list-context
    (let [attachment (find-attachment "Receipt")
          response (-> (req/request :delete (path :api
                                                  :attachments
                                                  (:id attachment)))
                       (edn-body (assoc attachment :caption "Updated caption"))
                       (add-auth (find-user email))
                       app)]
      [response (models/find attachment)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The attachment cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The attachment can be retrieved after a blocked delete"))

(deftest a-user-can-delete-an-attachment-in-his-entity
  (assert-successful-delete (delete-attachment "john@doe.com")))

(deftest a-user-cannot-delete-an-attachment-in-anothers-entity
  (assert-blocked-delete (delete-attachment "jane@doe.com")))
