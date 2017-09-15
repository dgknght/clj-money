(ns clj-money.authorization-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [cemerick.friend :refer [current-authentication]]
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

(def authorization-context
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

(deftest entity-management
  (let [context (serialization/realize storage-spec authorization-context)
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

(deftest account-management
  (let [context (serialization/realize storage-spec authorization-context)
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
  (let [context (serialization/realize storage-spec authorization-context)
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
