(ns clj-money.api.transactions-test
  (:require [clojure.test :refer [use-fixtures deftest is]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-entity
                                            find-account
                                            find-transaction]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.models.transactions :as trans]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}]
   :transactions [{:description "Paycheck"
                   :transaction-date (t/local-date 2016 2 1)
                   :memo "Pre-existing transaction"
                   :items [{:account-id "Checking"
                            :action :debit
                            :quantity 1000M
                            :memo "checking item"}
                           {:account-id "Salary"
                            :action :credit
                            :quantity 1000M
                            :memo "salary item"}]}]})

(defn- get-a-list
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")]
    (-> (req/request :get (path :api
                                :entities
                                (:id entity)
                                "2016-02-01"
                                "2016-02-29"
                                :transactions))
        (add-auth user)
        (req/header "Accept" "application/edn")
        app
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:transaction-date (t/local-date 2016 2 1)
                           :description "Paycheck"
                           :memo "Pre-existing transaction"
                           :value 1000.0M}]
                         edn-body)
      "The correct transactions are returned in the response"))

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
  (let [ctx (realize context)
        user (find-user ctx email)
        transaction (find-transaction ctx (t/local-date 2016 2 1) "Paycheck")]
    (-> (req/request :get (path :api
                                :transactions
                                "2016-02-01"
                                (:id transaction)))
        (add-auth user)
        (req/header "Accept" "application/edn")
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (comparable? {:transaction-date (t/local-date 2016 2 1)
                    :description "Paycheck"
                    :memo "Pre-existing transaction"
                    :value 1000.0M}
                   edn-body)
      "The correct transaction is returned in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-transaction-from-his-entity
  (assert-successful-get (get-a-transaction "john@doe.com")))

(deftest a-user-cannot-get-a-transaction-from-anothers-entity
  (assert-blocked-get (get-a-transaction "jane@doe.com")))

(defn- create-a-simple-transaction
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        checking (find-account ctx "Checking")
        salary (find-account ctx "Salary")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :transactions))
                     (edn-body {:description "Paycheck"
                                :transaction-date (t/local-date 2016 3 2)
                                :memo "Seems like there should be more"
                                :debit-account-id (:id checking)
                                :credit-account-id (:id salary)
                                :quantity 1000M})
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (trans/search {:entity-id (:id entity)
                                 :transaction-date (t/local-date 2016 3 2)})]
    [response retrieved]))

(defn- create-a-transaction
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        checking (find-account ctx "Checking")
        salary (find-account ctx "Salary")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :transactions))
                     (edn-body {:description "Paycheck"
                                :transaction-date (t/local-date 2016 3 2)
                                :memo "Seems like there should be more"
                                :items [{:account-id (:id checking)
                                         :action :debit
                                         :quantity 1000.0M
                                         :memo "checking item"}
                                        {:account-id (:id salary)
                                         :action :credit
                                         :quantity 1000.0M
                                         :memo "salary item"}]})
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (trans/search {:entity-id (:id entity)
                                 :transaction-date (t/local-date 2016 3 2)})]
    [response retrieved]))

(defn- assert-successful-create
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected {:description "Paycheck"

                  :transaction-date (t/local-date 2016 3 2)
                  :memo "Seems like there should be more"}]
    (is (comparable? expected edn-body)
        "The created transaction is returned in the response")
    (is (seq-with-map-like? expected retrieved)
        "The created transaction can be retrieved from the database")))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (seq-with-no-map-like? {:description "Paycheck"
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

(defn- update-items
  [items]
  (->> items
       (map #(select-keys % [:quantity :account-id :action :memo]))))

(defn- update-a-transaction
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        transaction (find-transaction ctx (t/local-date 2016 2 1) "Paycheck")
        response (-> (req/request :patch (path :api
                                               :transactions
                                               (serialize-local-date (:transaction-date transaction))
                                               (:id transaction)))
                     (edn-body (-> transaction
                                        (assoc :description "Just got paid today")
                                        (update-in [:items] update-items)))
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (trans/find (:id transaction) (:transaction-date transaction))]
    [response retrieved]))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (let [expected {:description "Just got paid today"
                  :transaction-date (t/local-date 2016 2 1)
                  :memo "Pre-existing transaction"}]
    (is (comparable? expected edn-body)
        "The updated transaction is returned in the response")
    (is (comparable? expected retrieved)
        "The transaction is updated in the database")))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:description "Paycheck"
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
  (let [ctx (realize context)
        user (find-user ctx email)
        transaction (find-transaction ctx (t/local-date 2016 2 1) "Paycheck")
        response (-> (req/request :delete (path :api
                                                :transactions
                                                "2016-02-01"
                                                (:id transaction)))
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (trans/find transaction)]
    [response retrieved]))

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
