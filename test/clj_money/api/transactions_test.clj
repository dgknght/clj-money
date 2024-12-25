(ns clj-money.api.transactions-test
  (:require [clojure.test :refer [use-fixtures deftest is]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test :refer [parse-json-body]]
            [dgknght.app-lib.test-assertions]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-entity
                                            find-account
                                            find-transaction]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  (conj (mapv #(factory :user {:user/email %})
              ["john@doe.com" "jane@doe.com"])
        #:entity{:name "Personal"
                 :user "john@doe.com"}
        #:entity{:name "Business"
                 :user "jane@doe.com"}
        #:commodity{:name "US Dollar"
                    :entity "Personal"
                    :symbol "USD"
                    :type :currency}
        #:account{:name "Checking"
                  :entity "Personal"
                  :type :asset}
        #:account{:name "Salary"
                  :entity "Personal"
                  :type :income}
        #:transaction{:description "Paycheck"
                      :entity "Personal"
                      :transaction-date (t/local-date 2016 2 1)
                      :memo "Pre-existing transaction"
                      :items [#:transaction-item{:account "Checking"
                                                 :action :debit
                                                 :quantity 1000M
                                                 :memo "checking item"}
                              #:transaction-item{:account "Salary"
                                                 :action :credit
                                                 :quantity 1000M
                                                 :memo "salary item"}]}))

(defn- get-a-list
  [email]
  (with-context context
    (-> (req/request :get (path :api
                                :entities
                                (:id (find-entity "Personal"))
                                "2016-02-01"
                                "2016-02-29"
                                :transactions))
        (add-auth (find-user email))
        app
        parse-json-body)))

(defn- assert-successful-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:transaction{:transaction-date "2016-02-01"
                                        :description "Paycheck"
                                        :memo "Pre-existing transaction"
                                        :value 1000.0}]
                         json-body)
      "The response contains the transaction for the specified entity in the specified date range"))

(defn- assert-blocked-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (empty? json-body) "The body is empty"))

(deftest a-user-can-get-transactions-in-his-entity
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-transactions-in-anothers-entity
  (assert-blocked-list (get-a-list "jane@doe.com")))

(defn- get-a-transaction
  [email]
  (with-context context
    (let [transaction (find-transaction [(t/local-date 2016 2 1) "Paycheck"])]
      (-> (req/request :get (path :api
                                  :transactions
                                  "2016-02-01"
                                  (:id transaction)))
          (add-auth (find-user email))
          app
          parse-json-body))))

(defn- assert-successful-get
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (comparable? #:transaction{:transaction-date "2016-02-01"
                                 :description "Paycheck"
                                 :memo "Pre-existing transaction"
                                 :value 1000.0}
                   json-body)
      "The response body contains the transaction details"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-transaction-from-his-entity
  (assert-successful-get (get-a-transaction "john@doe.com")))

(deftest a-user-cannot-get-a-transaction-from-anothers-entity
  (assert-blocked-get (get-a-transaction "jane@doe.com")))

(defn- create-a-simple-transaction
  [email]
  (with-context context
    (let [entity (find-entity "Personal")
          checking (find-account "Checking")
          salary (find-account "Salary")
          response (-> (req/request :post (path :api
                                                :entities
                                                (:id entity)
                                                :transactions))
                       (req/json-body #:transaction{:description "Paycheck"
                                                    :transaction-date "2016-03-02"
                                                    :memo "Seems like there should be more"
                                                    :debit-account {:id (:id checking)}
                                                    :credit-account {:id (:id salary)}
                                                    :quantity 1000M})
                       (add-auth (find-user email))
                       app
                       parse-json-body)
          retrieved (models/select #:transaction{:entity entity
                                                 :transaction-date (t/local-date 2016 3 2)})]
      [response retrieved])))

(defn- create-a-transaction
  [email]
  (with-context context
    (let [entity (find-entity "Personal")
          checking (find-account "Checking")
          salary (find-account "Salary")
          response (-> (req/request :post (path :api
                                                :entities
                                                (:id entity)
                                                :transactions))
                       (req/json-body
                         #:transaction{:description "Paycheck"
                                       :transaction-date "2016-03-02"
                                       :memo "Seems like there should be more"
                                       :items [#:transaction-item{:account (select-keys checking [:id])
                                                                  :action :debit
                                                                  :quantity 1000.0
                                                                  :memo "checking item"}
                                               #:transaction-item{:account (select-keys salary [:id])
                                                                  :action :credit
                                                                  :quantity 1000.0
                                                                  :memo "salary item"}]})
                       (add-auth (find-user email))
                       app
                       parse-json-body)]
      [response (models/select #:transaction{:entity entity
                                             :transaction-date (t/local-date 2016 3 2)})])))

(defn- assert-successful-create
  [[{:as response :keys [json-body]} retrieved]]
  (is (http-success? response))
  (is (comparable? #:transaction{:description "Paycheck"
                                 :transaction-date "2016-03-02"
                                 :memo "Seems like there should be more"}
                   json-body)
      "The created transaction is returned in the response")
  (is (seq-with-map-like? #:transaction{:description "Paycheck"

                                        :transaction-date (t/local-date 2016 3 2)
                                        :memo "Seems like there should be more"}
                          retrieved)
      "The created transaction can be retrieved from the database"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (seq-with-no-map-like? #:transaction{:description "Paycheck"
                                           :transaction-date (t/local-date 2016 3 2)
                                           :memo "Seems like there should be more"}
                             retrieved)
      "The transaction is not created"))

(deftest a-user-can-create-a-transaction-in-his-entity
  (assert-successful-create (create-a-transaction "john@doe.com")))

(deftest a-user-cannot-create-a-transaction-in-aothers-entity
  (assert-blocked-create (create-a-transaction "jane@doe.com")))

(deftest a-user-can-create-a-simple-transaction-in-his-entity
  (assert-successful-create (create-a-simple-transaction "john@doe.com")))

(deftest a-user-cannot-create-a-simple-transaction-in-aothers-entity
  (assert-blocked-create (create-a-simple-transaction "jane@doe.com")))

; (defn- update-items
;   [items]
;   (->> items
;        (map #(select-keys % [:quantity :account-id :action :memo]))))
; 
; (defn- update-a-transaction
;   [email]
;   (let [ctx (realize context)
;         user (find-user ctx email)
;         transaction (find-transaction ctx (t/local-date 2016 2 1) "Paycheck")
;         response (-> (req/request :patch (path :api
;                                                :transactions
;                                                (serialize-local-date (:transaction-date transaction))
;                                                (:id transaction)))
;                      (req/json-body (-> transaction
;                                         (assoc :description "Just got paid today")
;                                         (update-in [:transaction-date] serialize-local-date)
;                                         (update-in [:items] update-items)))
;                      (add-auth user)
;                      app)
;         body (json/parse-string (:body response) true)
;         retrieved (trans/find (:id transaction) (:transaction-date transaction))]
;     [response body retrieved]))
; 
; (defn- assert-successful-update
;   [[response body retrieved]]
;   (is (http-success? response))
;   (is (comparable? {:description "Just got paid today"
;                     :transaction-date "2016-02-01"
;                     :memo "Pre-existing transaction"}
;                    body)
;       "The updated transaction is returned in the response")
;   (is (comparable? {:description "Just got paid today"
;                     :transaction-date (t/local-date 2016 2 1)
;                     :memo "Pre-existing transaction"}
;                    retrieved)
;       "The transaction is updated in the database"))
; 
; (defn- assert-blocked-update
;   [[response _ retrieved]]
;   (is (http-not-found? response))
;   (is (comparable? {:description "Paycheck"
;                     :transaction-date (t/local-date 2016 2 1)
;                     :memo "Pre-existing transaction"}
;                    retrieved)
;       "The transaction is not updated"))
; 
; (deftest a-user-can-update-a-transaction-in-his-entity
;   (assert-successful-update (update-a-transaction "john@doe.com")))
; 
; (deftest a-user-cannot-update-a-transaction-in-anothers-entity
;   (assert-blocked-update (update-a-transaction "jane@doe.com")))
; 
; (defn- delete-a-transaction
;   [email]
;   (let [ctx (realize context)
;         user (find-user ctx email)
;         transaction (find-transaction ctx (t/local-date 2016 2 1) "Paycheck")
;         response (-> (req/request :delete (path :api
;                                                 :transactions
;                                                 "2016-02-01"
;                                                 (:id transaction)))
;                      (add-auth user)
;                      app)
;         retrieved (trans/find transaction)]
;     [response retrieved]))
; 
; (defn- assert-successful-delete
;   [[response retrieved]]
;   (is (http-success? response))
;   (is (nil? retrieved)
;       "The record cannot be retrieved after delete"))
; 
; (defn- assert-blocked-delete
;   [[response retrieved]]
;   (is (http-not-found? response))
;   (is retrieved
;       "The record can be retrieved after a blocked delete"))
; 
; (deftest a-user-can-delete-a-transaction-in-his-entity
;   (assert-successful-delete (delete-a-transaction "john@doe.com")))
; 
; (deftest a-user-cannot-delete-a-transaction-in-anothers-entity
;   (assert-blocked-delete (delete-a-transaction "jane@doe.com")))
