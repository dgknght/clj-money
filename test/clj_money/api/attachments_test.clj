(ns clj-money.api.attachments-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.validation :as v]
            [dgknght.app-lib.test :refer [parse-json-body]]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.dates :as dates]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
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
                       app
                       parse-json-body)]
      [response
       (when-let [id (get-in response [:json-body :id])]
         (models/find id :attachment))])))

(defn- assert-successful-create
  [[{:keys [json-body] :as response} retrieved]]
  (is (http-created? response))
  (is (empty? (::v/errors json-body))
      "There are no validation errors")
  (is (:id json-body) "An ID is assigned to the new record")
  (is (comparable? {:attachment/transaction-date (t/local-date 2015 1 1)}
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
                :body (io/file (io/resource "fixtures/attachment.jpg"))}
        #:attachment{:caption "Receipt"
                     :transaction [(t/local-date 2015 1 1)
                                   "Paycheck"]
                     :image "receipt.jpg"}))

(defn- list-attachments
  [email]
  (with-context list-context
    (let [transaction (find-transaction [(t/local-date 2015 1 1)
                                         "Paycheck"])]
      (-> (req/request :get (str (path :api
                                       :attachments)
                                 "?"
                                 (map->query-string
                                   {:transaction-date-on-or-after "2015-01-01"
                                    :transaction-date-on-or-before "2015-01-31"
                                    :transaction-id (:id transaction)})))
          (add-auth (find-user email))
          app
          parse-json-body))))

(defn- assert-successful-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:attachment/caption "Receipt"}]
                         json-body)
      "The correct content is returned."))

(defn- assert-blocked-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (empty? json-body) "No records are returned"))

(deftest a-user-can-get-a-list-of-attachments-in-his-entity
  (assert-successful-list (list-attachments "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-attachments-in-anothers-entity
  (assert-blocked-list (list-attachments "jane@doe.com")))

; (defn- update-attachment
;   [email]
;   (let [ctx (realize list-context)
;         attachment (find-attachment ctx "Receipt")
;         user (find-user ctx email)
;         response (-> (req/request :patch (path :api
;                                                :attachments
;                                                (:id attachment)))
;                      (req/json-body (assoc attachment :caption "Updated caption"))
;                      (add-auth user)
;                      app)
;         body (json/parse-string (:body response) true)
;         retrieved (att/find attachment)]
;     [response body retrieved]))
; 
; (defn- assert-successful-update
;   [[response body retrieved]]
;   (is (http-success? response))
;   (is (= {:caption "Updated caption"}
;          (select-keys body [:caption]))
;       "The updated attachment is returned")
;   (is (= {:caption "Updated caption"}
;          (select-keys retrieved [:caption]))
;       "The database is updated"))
; 
; (defn- assert-blocked-update
;   [[response _ retrieved]]
;   (is (http-not-found? response))
;   (is (= {:caption "Receipt"}
;          (select-keys retrieved [:caption]))
;       "The database is not updated"))
; 
; (deftest a-user-can-update-an-attachment-in-his-entity
;   (assert-successful-update (update-attachment "john@doe.com")))
; 
; (deftest a-user-cannot-update-an-attachment-in-anothers-entity
;   (assert-blocked-update (update-attachment "jane@doe.com")))
; 
; (defn- delete-attachment
;   [email]
;   (let [ctx (realize list-context)
;         attachment (find-attachment ctx "Receipt")
;         user (find-user ctx email)
;         response (-> (req/request :delete (path :api
;                                                 :attachments
;                                                 (:id attachment)))
;                      (req/json-body (assoc attachment :caption "Updated caption"))
;                      (add-auth user)
;                      app)
;         retrieved (att/find attachment)]
;     [response retrieved]))
; 
; (defn- assert-successful-delete
;   [[response retrieved]]
;   (is (http-success? response))
;   (is (nil? retrieved) "The attachment cannot be retrieved after delete"))
; 
; (defn- assert-blocked-delete
;   [[response retrieved]]
;   (is (http-not-found? response))
;   (is retrieved "The attachment can be retrieved after a blocked delete"))
; 
; (deftest a-user-can-delete-an-attachment-in-his-entity
;   (assert-successful-delete (delete-attachment "john@doe.com")))
; 
; (deftest a-user-cannot-delete-an-attachment-in-anothers-entity
;   (assert-blocked-delete (delete-attachment "jane@doe.com")))
