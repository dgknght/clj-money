(ns clj-money.permissions.accounts-test
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
            [clj-money.models.accounts :as accounts]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.accounts]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-account
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def accounts-context
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
               :entity-id "Business"}]})

(deftest account-list
  (let [context (serialization/realize storage-spec accounts-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (find-entity context "Personal")]
    (testing "A user has permission to list accounts in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:entity-id (:id entity)} :account)
                         (accounts/search storage-spec)
                         count))
            "The accounts are returned")))
    (testing "A user does not have permission list accounts in someone else's entity"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                     (->> (apply-scope {:entity-id (:id entity)} :account)
                          (accounts/search storage-spec)
                          count)))))
    (testing "A user can be granted permission to list accounts in someone else's entity"
      (grants/create storage-spec {:entity-id (:id entity)
                                   :user-id (:id jane)
                                   :permissions {:account #{:index}}})
      (with-authentication jane
        (is (not= 0 (->> (apply-scope {:entity-id (:id entity)} :account)
                         (accounts/search storage-spec)
                         count))
            "The accounts are returned")))))

(deftest account-management
  (let [context (serialization/realize storage-spec accounts-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        checking (find-account context "Checking")]
    (testing "A user has permission on accounts in his own entities"
      (with-authentication john
        (doseq [action [:show :update :delete]]
          (is (allowed? action checking)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on accounts in someone else's entity"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action checking))
              (format "A user does not have %s permission" action)))))
    (testing "A user can be granted permission on accounts in someone else's entity"
      (with-authentication jane
        (grants/create storage-spec {:user-id (:id jane)
                                     :entity-id (:entity-id checking)
                                     :permissions {:account #{:show}} })
        (doseq [action [:show]]
          (is (allowed? action checking)
              (format "A user has %s permission" action)))
        (doseq [action [:edit :update :delete]]
          (is (not (allowed? action checking))
              (format "A user does not have %s permission" action)))))))

(deftest account-creation
  (let [context (serialization/realize storage-spec accounts-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        savings (tag-resource {:name "Salary"
                               :type :income
                               :entity-id (:id personal)}
                              :account)]
    (testing "A user has permission to create an account in his own entities"
      (with-authentication john
        (is (allowed? :create savings)
            "Create is allowed")))
    (testing "A user does not have permission to create an account in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create savings))
            "Create is not allowed")))))

