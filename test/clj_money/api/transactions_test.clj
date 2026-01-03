(ns clj-money.api.transactions-test
  (:require [clojure.test :refer [use-fixtures deftest is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [uri map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.formats :as fmt]
            [clj-money.json]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.api.test-helper :refer [parse-body
                                               request
                                               jsonize-decimals]]
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
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (-> (path :api
                              :entities
                              (:id (find-entity "Personal"))
                              :transactions)
                        uri
                        (assoc :query
                               (map->query-string
                                 {:transaction-date-on-or-after "2016-02-01"
                                  :transaction-date-before "2016-03-01"}))
                        str)
               :content-type content-type
               :user (find-user email))
      app
      parse-body))

(defn- assert-successful-list
  [{:as response :keys [parsed-body]}
   & {:keys [expected]
      :or {expected [#:transaction{:transaction-date (t/local-date 2016 02 01)
                                   :description "Paycheck"
                                   :memo "Pre-existing transaction"
                                   :value 1000.0M}]}}]
  (is (http-success? response))
  (is (seq-of-maps-like? expected
                         (jsonize-decimals parsed-body))
      "The response contains the transaction for the specified entity in the specified date range"))

(defn- assert-blocked-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (empty? parsed-body) "The body is empty"))

(deftest a-user-can-get-transactions-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-list (get-a-list "john@doe.com")))
    (testing "json format"
      (assert-successful-list
        (get-a-list "john@doe.com" :content-type "application/json")
        :expected [{:description "Paycheck"
                    :transactionDate "2016-02-01"
                    :memo "Pre-existing transaction"
                    :value "1,000.00"
                    :_type "transaction"}]))))

(deftest a-user-cannot-get-transactions-in-anothers-entity
  (with-context context
    (assert-blocked-list (get-a-list "jane@doe.com"))))

(defn- get-a-transaction
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [transaction (find-transaction [(t/local-date 2016 2 1) "Paycheck"])]
    (-> (request :get (path :api
                            :transactions
                            (:id transaction))
                 :content-type content-type
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-get
  [{:as response :keys [parsed-body]}
   & {:keys [expected]
      :or {expected #:transaction{:transaction-date (t/local-date 2016 02 01)
                                  :description "Paycheck"
                                  :memo "Pre-existing transaction"
                                  :value 1000.0M}}}]
  (is (http-success? response))
  (is (comparable? expected (jsonize-decimals parsed-body))
      "The response body contains the transaction details"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-transaction-from-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-get (get-a-transaction "john@doe.com")))
    (testing "json format"
      (assert-successful-get
        (get-a-transaction "john@doe.com" :content-type "application/json")
        :expected {:transactionDate "2016-02-01"
                   :description "Paycheck"
                   :memo "Pre-existing transaction"
                   :value "1,000.00"}))))

(deftest a-user-cannot-get-a-transaction-from-anothers-entity
  (with-context context
    (assert-blocked-get (get-a-transaction "jane@doe.com"))))

(defn- create-a-simple-transaction
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [entity (find-entity "Personal")
        checking (find-account "Checking")
        salary (find-account "Salary")
        body #:transaction{:description "Paycheck"
                           :transaction-date (t/local-date 2016 03 02)
                           :memo "Seems like there should be more"
                           :debit-account {:id (:id checking)}
                           :credit-account {:id (:id salary)}
                           :quantity 1000M}
        response (-> (request :post (path :api
                                         :entities
                                         (:id entity)
                                         :transactions)
                              :user (find-user email)
                              :content-type content-type
                              :body body)
                     app
                     parse-body)
        retrieved (entities/select #:transaction{:entity entity
                                                :transaction-date (t/local-date 2016 3 2)})]
    [response retrieved]))

(defn- create-a-transaction
  [email & {:keys [content-type body]
            :or {content-type "application/edn"}}]
  (let [entity (find-entity "Personal")
        checking (find-account "Checking")
        salary (find-account "Salary")
        default-body #:transaction{:description "Paycheck"
                                   :transaction-date (t/local-date 2016 03 02)
                                   :memo "Seems like there should be more"
                                   :items [#:transaction-item{:account (util/->entity-ref checking)
                                                              :action :debit
                                                              :quantity 1000M
                                                              :memo "checking item"}
                                           #:transaction-item{:account (util/->entity-ref salary)
                                                              :action :credit
                                                              :quantity 1000M
                                                              :memo "salary item"}]}
        request-body (or body default-body)
        response (-> (request :post (path :api
                                         :entities
                                         (:id entity)
                                         :transactions)
                              :user (find-user email)
                              :content-type content-type
                              :body request-body)
                     app
                     parse-body)]
    [response (entities/select #:transaction{:entity entity
                                           :transaction-date (t/local-date 2016 3 2)})]))

(defn- assert-successful-create
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected-response]
      :or {expected-response #:transaction{:description "Paycheck"
                                           :transaction-date (t/local-date 2016 3 2)
                                           :memo "Seems like there should be more"}}}]
  (is (http-success? response))
  (is (comparable? expected-response parsed-body)
      "The created transaction is returned in the response")
  (let [expected #:transaction{:description "Paycheck"
                               :transaction-date (t/local-date 2016 3 2)
                               :memo "Seems like there should be more"}]
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
  (with-context context
    (testing "default format (edn)"
      (assert-successful-create (create-a-transaction "john@doe.com")))
    (testing "json format"
      (let [checking (find-account "Checking")
            salary (find-account "Salary")]
        (assert-successful-create
          (create-a-transaction "john@doe.com"
                                :content-type "application/json"
                                :body {:description "Paycheck"
                                       :transactionDate "2016-03-02"
                                       :memo "Seems like there should be more"
                                       :items [{:account {:id (:id checking)}
                                                :action "debit"
                                                :quantity 1000
                                                :memo "checking item"
                                                :_type "transaction-item"}
                                               {:account {:id (:id salary)}
                                                :action "credit"
                                                :quantity 1000
                                                :memo "salary item"
                                                :_type "transaction-item"}]
                                       :_type "transaction"})
          :expected-response {:description "Paycheck"
                              :transactionDate "2016-03-02"
                              :memo "Seems like there should be more"
                              :_type "transaction"})))))

(deftest a-user-cannot-create-a-transaction-in-aothers-entity
  (with-context context
    (assert-blocked-create (create-a-transaction "jane@doe.com"))))

(deftest a-user-can-create-a-simple-transaction-in-his-entity
  (with-context context
    (assert-successful-create (create-a-simple-transaction "john@doe.com"))))

(deftest a-user-cannot-create-a-simple-transaction-in-aothers-entity
  (with-context context
    (assert-blocked-create (create-a-simple-transaction "jane@doe.com"))))

(defn- update-a-transaction
  [email & {:keys [content-type body]
            :or {content-type "application/edn"}}]
  (let [transaction (find-transaction [(t/local-date 2016 2 1)
                                       "Paycheck"])
        response (-> (request :patch (path :api
                                           :transactions
                                           (:id transaction))
                              :user (find-user email)
                              :content-type content-type
                              :body (or body
                                        (merge transaction
                                               {:transaction/description
                                                "Just got paid today"})))
                     app
                     parse-body)]
    [response (entities/find-by
                (select-keys transaction
                             [:id
                              :transaction/transaction-date]))]))

(defn- assert-successful-update
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected
             expected-response]
      :or {expected #:transaction{:description "Just got paid today"
                                  :transaction-date (t/local-date 2016 2 1)
                                  :memo "Pre-existing transaction"}}}]
  (is (http-success? response))
  (is (comparable? (or expected-response
                       expected)
                   parsed-body)
      "The updated transaction is returned in the response")
  (is (comparable? expected retrieved)
      "The transaction is updated in the database"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? #:transaction{:description "Paycheck"
                                 :transaction-date (t/local-date 2016 2 1)
                                 :memo "Pre-existing transaction"}
                   retrieved)
      "The transaction is not updated"))

(deftest a-user-can-update-a-transaction-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-update (update-a-transaction "john@doe.com")))
    (testing "json format"
      (let [transaction (find-transaction [(t/local-date 2016 2 1) "Paycheck"])]
        (assert-successful-update
          (update-a-transaction "john@doe.com"
                                :content-type "application/json"
                                :body (-> transaction
                                          fmt/edn->json
                                          (assoc :description "Got me a pocket full of change")))
          :expected #:transaction{:description "Got me a pocket full of change"
                                  :transaction-date (t/local-date 2016 2 1)
                                  :memo "Pre-existing transaction"}
          :expected-response {:description "Got me a pocket full of change"
                              :transactionDate "2016-02-01"
                              :memo "Pre-existing transaction"
                              :_type "transaction"})))))

(deftest a-user-cannot-update-a-transaction-in-anothers-entity
  (with-context context
    (assert-blocked-update (update-a-transaction "jane@doe.com"))))

(defn- delete-a-transaction
  [email]
  (with-context context
    (let [transaction (find-transaction [(t/local-date 2016 2 1) "Paycheck"])
          response (-> (request :delete (path :api
                                             :transactions
                                             (:id transaction))
                                :user (find-user email))
                       app)]
      [response (entities/find transaction)])))

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
