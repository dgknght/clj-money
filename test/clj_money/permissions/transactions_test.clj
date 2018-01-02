(ns clj-money.permissions.transactions-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [slingshot.test :refer :all]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.authorization :refer [apply-scope
                                             allowed?
                                             tag-resource]]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.transactions]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-account
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def transactions-context
  {:users [(assoc (factory :user) :email "john@doe.com")
           (assoc (factory :user) :email "jane@doe.com")]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}]
   :accounts [{:name "Checking"
               :type :asset
               :entity-id "Personal"}
              {:name "Savings"
               :type :asset
               :entity-id "Business"}
              {:name "Salary"
               :type :income
               :entity-id "Personal"}]
   :transactions [{:transaction-date (t/local-date 2017 3 2)
                   :description "Paycheck"
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000M}]}]})

(deftest transaction-list
  (let [context (serialization/realize storage-spec transactions-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (find-entity context "Personal")]
    (testing "A user has permission to list transactions in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:entity-id (:id entity)
                                       :transaction-date [:between
                                                          (t/local-date 2015 1 1)
                                                          (t/local-date 2017 12 31)]}
                                      :transaction)
                         (transactions/search storage-spec)
                         count))
            "The transactions are returned")))
    (testing "A user does not have permission list transactions in someone else's transaction"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                      (->> (apply-scope {:entity-id (:id entity)} :transaction)
                           (transactions/search storage-spec)
                           count)))))))

(deftest transaction-management
  (let [context (serialization/realize storage-spec transactions-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        transaction (-> context :transactions first)]
    (testing "A user has permissions on transactions in his own entities"
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action transaction)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permissions on transactions in someone else's entities"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action transaction))
              (format "A user does not have  %s permission" action)))))
    (testing "A user can be granted permissions on transactions in someone else's entities"
      (grants/create storage-spec {:user-id (:id jane)
                                   :entity-id (:entity-id transaction)
                                   :permissions {:transaction #{:show}}})
      (with-authentication jane
        (doseq [action [:show]]
          (is (allowed? action transaction)
              (format "A user has %s permission" action)))
        (doseq [action [:edit :update :delete]]
          (is (not (allowed? action transaction))
              (format "A user does not have  %s permission" action)))))))

(deftest transaction-creation
  (let [context (serialization/realize storage-spec transactions-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        transaction (tag-resource {:transaction-date (t/today)
                                   :description "Paycheck"
                                   :entity-id (:id personal)
                                   :items [{:action :debit
                                            :account-id "Checking"
                                            :amount 1000M}
                                           {:action :credit
                                            :account-id "Salary"
                                            :amount 1000M}]}
                                  :transaction)]
    (testing "A user has permission to create a transactions in his own entities"
      (with-authentication john
        (is (allowed? :create transaction)
            "Create is allowed")))
    (testing "A user does not have permission to create a transaction in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create transaction))
            "Create is not allowed")))))

