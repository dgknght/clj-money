(ns clj-money.api.transactions-test
  (:require [clojure.test :refer [use-fixtures deftest is]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
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
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
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
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:transaction{:transaction-date (t/local-date 2016 02 01)
                                        :description "Paycheck"
                                        :memo "Pre-existing transaction"
                                        :value 1000.0M}]
                         edn-body)
      "The response contains the transaction for the specified entity in the specified date range"))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "The body is empty"))

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
          parse-edn-body))))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (comparable? #:transaction{:transaction-date (t/local-date 2016 02 01)
                                 :description "Paycheck"
                                 :memo "Pre-existing transaction"
                                 :value 1000.0M}
                   edn-body)
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
                       (edn-body #:transaction{:description "Paycheck"
                                                    :transaction-date (t/local-date 2016 03 02)
                                                    :memo "Seems like there should be more"
                                                    :debit-account {:id (:id checking)}
                                                    :credit-account {:id (:id salary)}
                                                    :quantity 1000M})
                       (add-auth (find-user email))
                       app
                       parse-edn-body)
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
                       (edn-body
                         #:transaction{:description "Paycheck"
                                       :transaction-date (t/local-date 2016 03 02)
                                       :memo "Seems like there should be more"
                                       :items [#:transaction-item{:account (util/->model-ref checking)
                                                                  :action :debit
                                                                  :quantity 1000M
                                                                  :memo "checking item"}
                                               #:transaction-item{:account (util/->model-ref salary)
                                                                  :action :credit
                                                                  :quantity 1000M
                                                                  :memo "salary item"}]})
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (models/select #:transaction{:entity entity
                                             :transaction-date (t/local-date 2016 3 2)})])))

(defn- assert-successful-create
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected #:transaction{:description "Paycheck"
                               :transaction-date (t/local-date 2016 3 2)
                               :memo "Seems like there should be more"}]
    (is (comparable? expected edn-body)
        "The created transaction is returned in the response")
    (is (seq-with-map-like? expected retrieved)
        "The created transaction can be retrieved from the database")))

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

(defn- update-a-transaction
  [email]
  (with-context context
    (let [transaction (find-transaction [(t/local-date 2016 2 1) "Paycheck"])
          response (-> (req/request :patch (path :api
                                                 :transactions
                                                 (serialize-local-date (:transaction/transaction-date transaction))
                                                 (:id transaction)))
                       (edn-body (assoc transaction
                                        :transaction/description
                                        "Just got paid today"))
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (models/find-by
                  (select-keys transaction
                               [:id
                                :transaction/transaction-date]))])))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected #:transaction{:description "Just got paid today"
                               :transaction-date (t/local-date 2016 2 1)
                               :memo "Pre-existing transaction"}]
    (is (comparable? expected edn-body)
        "The updated transaction is returned in the response")
    (is (comparable? expected retrieved)
        "The transaction is updated in the database")))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? #:transaction{:description "Paycheck"
                                 :transaction-date (t/local-date 2016 2 1)
                                 :memo "Pre-existing transaction"}
                   retrieved)
      "The transaction is not updated"))

(deftest a-user-can-update-a-transaction-in-his-entity
  (assert-successful-update (update-a-transaction "john@doe.com")))

(deftest a-user-cannot-update-a-transaction-in-anothers-entity
  (assert-blocked-update (update-a-transaction "jane@doe.com")))

(defn- delete-a-transaction
  [email]
  (with-context context
    (let [transaction (find-transaction [(t/local-date 2016 2 1) "Paycheck"])
          response (-> (req/request :delete (path :api
                                                  :transactions
                                                  "2016-02-01"
                                                  (:id transaction)))
                       (add-auth (find-user email))
                       app)]
      [response (models/find transaction)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved)
      "The record cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved
      "The record can be retrieved after a blocked delete"))

(deftest a-user-can-delete-a-transaction-in-his-entity
  (assert-successful-delete (delete-a-transaction "john@doe.com")))

(deftest a-user-cannot-delete-a-transaction-in-anothers-entity
  (assert-blocked-delete (delete-a-transaction "jane@doe.com")))
