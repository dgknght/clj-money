(ns clj-money.api.attachments-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.dates :as dates]
            [clj-money.test-helpers :refer [reset-db
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               request
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-transaction
                                            find-attachment]]
            [clj-money.entities :as entities]
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
         (entities/find id :attachment))])))

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
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (with-context list-context
    (let [transaction (find-transaction [(t/local-date 2015 1 1)
                                         "Paycheck"])]
      (-> (request :get (path :api
                              :transactions
                              (:id transaction)
                              (dates/serialize-local-date
                                (:transaction/transaction-date transaction))
                              :attachments)
                   :content-type content-type
                   :user (find-user email))
          app
          parse-body))))

(defn- list-account-attachments
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (with-context list-context
    (let [account (find-account "Checking")]
      (-> (request :get (path :api
                              :accounts
                              (:id account)
                              :attachments
                              "2015-01-01"
                              "2015-02-01")
                   :content-type content-type
                   :user (find-user email))
          app
          parse-body))))

(defn- assert-successful-list
  [{:as response :keys [edn-body parsed-body]}
   & {:keys [expected]
      :or {expected [{:attachment/caption "Receipt"}]}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (seq-of-maps-like? expected body)
        "The list of attachments is returned.")))

(defn- assert-blocked-list
  [{:as response :keys [edn-body parsed-body]}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (empty? body) "No records are returned")))

(deftest a-user-can-get-a-list-of-attachments-for-a-trx-in-his-entity
  (assert-successful-list (list-trx-attachments "john@doe.com"))
  (assert-successful-list (list-trx-attachments "john@doe.com" :content-type "application/json")
                          :expected [{:caption "Receipt"
                                      :_type "attachment"}]))

(deftest a-user-cannot-get-a-list-of-attachments-for-a-trx-in-anothers-entity
  (assert-blocked-list (list-trx-attachments "jane@doe.com")))

(deftest a-user-can-get-a-list-of-attachments-for-an-account-in-his-entity
  (assert-successful-list (list-account-attachments "john@doe.com"))
  (assert-successful-list (list-account-attachments "john@doe.com" :content-type "application/json")
                          :expected [{:caption "Receipt"
                                      :_type "attachment"}]))

(deftest a-user-cannot-get-a-list-of-attachments-for-an-account-in-anothers-entity
  (assert-blocked-list (list-account-attachments "jane@doe.com")))

(defn- update-attachment
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body nil}}]
  (with-context list-context
    (let [attachment (find-attachment "Receipt")
          default-body (assoc attachment :attachment/caption "Updated caption")
          response (-> (request :patch (path :api
                                            :attachments
                                            (:id attachment))
                               :content-type content-type
                               :body (or body default-body)
                               :user (find-user email))
                       app
                       parse-body)]
      [response (entities/find attachment)])))

(defn- assert-successful-update
  [[{:as response :keys [edn-body parsed-body]} retrieved]
   & {:keys [expected]
      :or {expected {:attachment/caption "Updated caption"}}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (comparable? expected body)
        "The updated attachment is returned"))
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
  (assert-successful-update (update-attachment "john@doe.com"))
  (assert-successful-update (update-attachment "john@doe.com"
                                              :content-type "application/json"
                                              :body {:caption "Updated caption"
                                                     :_type "attachment"})
                           :expected {:caption "Updated caption"
                                      :_type "attachment"}))

(deftest a-user-cannot-update-an-attachment-in-anothers-entity
  (assert-blocked-update (update-attachment "jane@doe.com")))

(defn- delete-attachment
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (with-context list-context
    (let [attachment (find-attachment "Receipt")
          response (-> (request :delete (path :api
                                              :attachments
                                              (:id attachment))
                               :content-type content-type
                               :user (find-user email))
                       app)]
      [response (entities/find attachment)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The attachment cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The attachment can be retrieved after a blocked delete"))

(deftest a-user-can-delete-an-attachment-in-his-entity
  (assert-successful-delete (delete-attachment "john@doe.com"))
  (assert-successful-delete (delete-attachment "john@doe.com" :content-type "application/json")))

(deftest a-user-cannot-delete-an-attachment-in-anothers-entity
  (assert-blocked-delete (delete-attachment "jane@doe.com")))
