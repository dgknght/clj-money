(ns clj-money.api.attachments-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [clj-money.dates :as dates]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth
                                               build-multipart-request]]
            [clj-money.test-context :refer [basic-context
                                            realize
                                            find-user
                                            find-transaction
                                            find-attachment]]
            [clj-money.models.attachments :as att]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private att-context
  (assoc basic-context :transactions [{:transaction-date (t/local-date 2015 1 1)
                                       :description "Paycheck"
                                       :quantity 1000M
                                       :debit-account-id "Checking"
                                       :credit-account-id "Salary"}]))

(defn- create-attachment
  [email]
  (let [ctx (realize att-context)
        transaction (find-transaction ctx (t/local-date 2015 1 1) "Paycheck")
        user (find-user ctx email)
        file (io/file (io/resource "fixtures/attachment.jpg"))
        response (-> (req/request :post (path :api
                                              :transactions
                                              (:id transaction)
                                              (dates/serialize-local-date (:transaction-date transaction))
                                              :attachments))
                     (merge (build-multipart-request {:file {:file file
                                                             :content-type "image/jpg"}}))
                     (add-auth user)
                     (req/header "Accept" "application/edn")
                     app
                     parse-edn-body)
        retrieved (when-let [id (get-in response [:edn-body :id])]
                    (att/find id))]
    [response retrieved]))

(defn- assert-successful-create
  [[{:keys [edn-body] :as response} retrieved]]
  (is (http-created? response))
  (is (empty? (::v/errors edn-body))
      "There are no validation errors")
  (is (:id edn-body) "An ID is assigned to the new record")
  (is (comparable? {:transaction-date (t/local-date 2015 1 1)}
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
  (assoc att-context
         :images [{:user-id "john@doe.com"
                   :original-filename "receipt.jpg"
                   :content-type "image/jpg"
                   :body (io/file (io/resource "fixtures/attachment.jpg"))}]
         :attachments [{:caption "Receipt"
                        :transaction-id {:transaction-date (t/local-date 2015 1 1)
                                         :description "Paycheck"}
                        :image-id "receipt.jpg"}]))

(defn- list-attachments
  [email]
  (let [ctx (realize list-context)
        user (find-user ctx email)
        transaction (find-transaction ctx (t/local-date 2015 1 1) "Paycheck")]
    (-> (req/request :get (str (path :api
                                     :attachments)
                               "?"
                               (map->query-string
                                 {:transaction-date-on-or-after "2015-01-01"
                                  :transaction-date-on-or-before "2015-01-31"
                                  :transaction-id (:id transaction)})))
        (add-auth user)
        app
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:caption "Receipt"}]
                         edn-body)
      "The list of attachments is returned"))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "No records are returned"))

(deftest a-user-can-get-a-list-of-attachments-in-his-entity
  (assert-successful-list (list-attachments "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-attachments-in-anothers-entity
  (assert-blocked-list (list-attachments "jane@doe.com")))

(defn- update-attachment
  [email]
  (let [ctx (realize list-context)
        attachment (find-attachment ctx "Receipt")
        user (find-user ctx email)
        response (-> (req/request :patch (path :api
                                               :attachments
                                               (:id attachment)))
                     (edn-body (assoc attachment :caption "Updated caption"))
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (att/find attachment)]
    [response retrieved]))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (is (comparable? {:caption "Updated caption"}
                   edn-body)
      "The updated attachment is returned")
  (is (comparable? {:caption "Updated caption"}
                   retrieved)
      "The database is updated"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:caption "Receipt"}
                   retrieved)
      "The database is not updated"))

(deftest a-user-can-update-an-attachment-in-his-entity
  (assert-successful-update (update-attachment "john@doe.com")))

(deftest a-user-cannot-update-an-attachment-in-anothers-entity
  (assert-blocked-update (update-attachment "jane@doe.com")))

(defn- delete-attachment
  [email]
  (let [ctx (realize list-context)
        attachment (find-attachment ctx "Receipt")
        user (find-user ctx email)
        response (-> (req/request :delete (path :api
                                                :attachments
                                                (:id attachment)))
                     (edn-body (assoc attachment :caption "Updated caption"))
                     (add-auth user)
                     app)
        retrieved (att/find attachment)]
    [response retrieved]))

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
