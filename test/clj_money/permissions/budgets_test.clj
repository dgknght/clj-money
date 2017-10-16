(ns clj-money.permissions.budgets-test
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
            [clj-money.models.budgets :as budgets]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.budgets]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-account
                                            find-budget
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private budgets-context
  {:users [(assoc (factory :user) :email "john@doe.com")
           (assoc (factory :user) :email "jane@doe.com")]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}]  
   :budgets [{:name "2017"
              :entity-id "Personal"
              :start-date (t/local-date 2017 1 1)
              :period :month
              :period-count 12
              :items [{:account-id "Income"
                       :periods (repeat 12 100M)
                       :start-date (t/local-date 2018 1 1)}]}]
   :accounts [{:name "Checking"
               :type :asset
               :entity-id "Personal"}
              {:name "Savings"
               :type :asset
               :entity-id "Business"}
              {:type :income
               :name "Income"}]})

(deftest budget-list
  (let [context (serialization/realize storage-spec budgets-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (find-entity context "Personal")]
    (testing "A user has permission to list budgets in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:entity-id (:id entity)} :budget)
                         (budgets/search storage-spec)
                         count))
            "The budgets are returned")))
    (testing "A user does not have permission list budgets in someone else's entity"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                      (->> (apply-scope{:entity-id (:id entity)} :budget)
                           (budgets/search storage-spec)
                           count))
            "An exception is thrown")))))

(deftest budget-creation
  (let [context (serialization/realize storage-spec budgets-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        budget (tag-resource {:entity-id (:id personal)
                              :name "2018"
                              :start-date (t/local-date 2018 1 1)}
                             :budget)]
    (testing "A user has permission to create an budget in his own entities"
      (with-authentication john
        (is (allowed? :create budget)
            "Create is allowed")))
    (testing "A user does not have permission to create an budget in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create budget))
            "Create is not allowed")))))

(deftest budget-management
  (let [context (serialization/realize storage-spec budgets-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        budget (find-budget context "2017")]
    (testing "A user has permission on budgets in his own entities"
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action budget)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on budgets in someone else's entity"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action budget))
              (format "A user does not have %s permission" action)))))))

(deftest budget-item-creation
  (let [context (serialization/realize storage-spec budgets-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        budget (find-budget context "2017")
        savings (find-account context "Savings")
        budget-item (tag-resource {:budget-id (:id budget)
                                   :account-id (:id savings)
                                   :periods (repeat 12 100M)
                                   :start-date (t/local-date 2018 1 1)}
                                  :budget-item)]
    (testing "A user has permission to create an item in budgets in his own entities"
      (with-authentication john
        (is (allowed? :create budget-item)
            "Create is allowed")))
    (testing "A user does not have permission to create an item in budgets in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create budget-item))
            "Create is not allowed")))))

(deftest budget-item-management
  (let [context (serialization/realize storage-spec budgets-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        budget-item (-> (find-budget context "2017")
                        :items
                        first)]
    (testing "A user has permission on items in budgets his own entities"
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action budget-item)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on items in budgets in someone else's entity"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action budget-item))
              (format "A user does not have %s permission" action)))))))
