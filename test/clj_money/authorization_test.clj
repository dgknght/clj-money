(ns clj-money.authorization-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [cemerick.friend :refer [current-authentication]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.authorization :refer [allowed?]]
            [clj-money.test-helpers :refer [reset-db
                                            find-account
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def entities-context
  {:users [(assoc (factory :user) :email "john@doe.com")
           (assoc (factory :user) :email "jane@doe.com")]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}]})

(deftest entity-management
  (let [context (serialization/realize storage-spec entities-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")]
    (testing "A user has permission on his own entities" 
      (with-redefs [current-authentication (fn [] john)]
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action personal {})
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on someone else's entiy"
      (with-redefs [current-authentication (fn [] jane)]
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action personal {}))
              (format "A user does not have %s permission" action)))))))

(def accounts-context
  (assoc entities-context :accounts [{:name "Checking"
                                      :type :asset
                                      :entity-id "Personal"}
                                     {:name "Savings"
                                      :type :asset
                                      :entity-id "Business"}]))

(deftest account-management
  (let [context (serialization/realize storage-spec accounts-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        checking (find-account context "Checking")]
    (testing "A user has permission on accounts in his own entities"
      (with-redefs [current-authentication (fn [] john)]
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action checking {})
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on accounts in someone else's entity"
      (with-redefs [current-authentication (fn [] jane)]
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action checking {}))
              (format "A user does not have %s permission" action)))))))

(deftest account-creation
  (let [context (serialization/realize storage-spec accounts-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        savings (with-meta {:name "Salary"
                            :type :income
                            :entity-id (:id personal)}
                           {:resource-type :account})]
    (testing "A user has permission to create an account in his own entities"
      (with-redefs [current-authentication (fn [] john)]
        (is (allowed? :create savings {})
            "Create is allowed")))
    (testing "A user does not have permission to create an account in someone else's entities"
      (with-redefs [current-authentication (fn [] jane)]
        (is (not (allowed? :create savings {}))
            "Create is not allowed")))))

; TODO develop a strategy to ensure index does not return records it should not

(def transactions-context
  (-> accounts-context
      (update-in [:accounts] #(conj % {:name "Salary"
                                       :type :income
                                       :entity-id "Personal"}))
      (assoc :transactions [{:transaction-date (t/local-date 2017 3 2)
                             :description "Paycheck"
                             :entity-id "Personal"
                             :items [{:action :debit
                                      :account-id "Checking"
                                      :amount 1000M}
                                     {:action :credit
                                      :account-id "Salary"
                                      :amount 1000M}]}])))

(deftest transaction-management
  (let [context (serialization/realize storage-spec transactions-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        transaction (-> context :transactions first)]
    (testing "A user has permissions on transactions in his own entities"
      (with-redefs [current-authentication (fn [] john)]
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action transaction {})
              (format "A user has %s permission" action)))))
    (testing "A user does not have permissions on transactions in someone else's entities"
      (with-redefs [current-authentication (fn [] jane)]
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action transaction {}))
              (format "A user does not have  %s permission" action)))))))

(deftest transaction-creation
  (let [context (serialization/realize storage-spec transactions-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        transaction (with-meta {:transaction-date (t/today)
                                :description "Paycheck"
                                :entity-id (:id personal)
                                :items [{:action :debit
                                         :account-id "Checking"
                                         :amount 1000M}
                                        {:action :credit
                                         :account-id "Salary"
                                         :amount 1000M}]}
                               {:resource-type :transaction})]
    (testing "A user has permission to create a transactions in his own entities"
      (with-redefs [current-authentication (fn [] john)]
        (is (allowed? :create transaction {})
            "Create is allowed")))
    (testing "A user does not have permission to create a transaction in someone else's entities"
      (with-redefs [current-authentication (fn [] jane)]
        (is (not (allowed? :create transaction {}))
            "Create is not allowed")))))
