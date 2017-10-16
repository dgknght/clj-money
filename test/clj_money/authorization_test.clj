(ns clj-money.authorization-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [slingshot.test :refer :all]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.authorization :refer [allowed?
                                             tag-resource
                                             apply-scope]]
            [clj-money.permissions.entities]
            [clj-money.permissions.grants]
            [clj-money.permissions.accounts]
            [clj-money.permissions.transactions]
            [clj-money.permissions.commodities]
            [clj-money.permissions.prices]
            [clj-money.permissions.budgets]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-account
                                            find-users
                                            find-entity
                                            find-grant
                                            find-budget
                                            find-commodity
                                            find-price]]))

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
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action personal)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on someone else's entiy"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action personal))
              (format "A user does not have %s permission" action)))))))

(deftest entity-creation
  (let [context (serialization/realize storage-spec entities-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (tag-resource {:name "Business"
                               :user-id (:id john)}
                             :entity)]
    (testing "A user has permission to create an entity for him/herself"
      (with-authentication john
        (is (allowed? :create entity)
            "Create is allowed")))
    (testing "A user does not have permission to create an entity for someone else"
      (with-authentication jane
        (is (not (allowed? :create entity))
            "Create is not allowed")))))

(def transactions-context
  (-> entities-context
      (assoc :accounts [{:name "Checking"
                         :type :asset
                         :entity-id "Personal"}
                        {:name "Savings"
                         :type :asset
                         :entity-id "Business"}
                        {:name "Salary"
                         :type :income
                         :entity-id "Personal"}])
      (assoc :transactions [{:transaction-date (t/local-date 2017 3 2)
                             :description "Paycheck"
                             :entity-id "Personal"
                             :items [{:action :debit
                                      :account-id "Checking"
                                      :amount 1000M}
                                     {:action :credit
                                      :account-id "Salary"
                                      :amount 1000M}]}])))

(deftest transaction-list
  (let [context (serialization/realize storage-spec transactions-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (find-entity context "Personal")]
    (testing "A user has permission to list transactions in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:entity-id (:id entity)} :transaction)
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

(deftest commodity-list
  (let [context (serialization/realize storage-spec entities-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (find-entity context "Personal")]
    (testing "A user has permission to list commodities in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:entity-id (:id entity)} :commodity)
                         (commodities/search storage-spec)
                         count))
            "The commodities are returned")))
    (testing "A user does not have permission list commodities in someone else's entity"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                     (->> (apply-scope {:entity-id (:id entity)} :commodity)
                          (commodities/search storage-spec)
                          count)))))))

(deftest commodity-creation
  (let [context (serialization/realize storage-spec entities-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        commodity (tag-resource {:entity-id (:id personal)
                                 :name "Apple, Inc."
                                 :symbol "AAPL"
                                 :type :stock}
                                :commodity)]
    (testing "A user has permission to create an commodity in his own entities"
      (with-authentication john
        (is (allowed? :create commodity)
            "Create is allowed")))
    (testing "A user does not have permission to create an commodity in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create commodity))
            "Create is not allowed")))))

(deftest commodity-management
  (let [context (serialization/realize storage-spec entities-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        commodity (find-commodity context "USD")]
    (testing "A user has permission on commodities in his own entities"
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action commodity)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on commodities in someone else's entity"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action commodity))
              (format "A user does not have %s permission" action)))))))

(def ^:private prices-context
  (-> entities-context
      (update-in [:commodities] #(conj % {:name "Apple, Inc."
                                          :symbol "AAPL"
                                          :type :stock
                                          :exchange :nasdaq
                                          :entity-id "Personal"}))
      (assoc :prices [{:commodity-id "AAPL"
                       :trade-date (t/local-date 2017 3 2)
                       :price 100M}])))

(deftest price-list
  (let [context (serialization/realize storage-spec prices-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        commodity (find-commodity context "AAPL")]
    (testing "A user has permission to list prices for commodities in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:commodity-id (:id commodity)} :price)
                         (prices/search storage-spec)
                         count))
            "The prices are returned")))
    (testing "A user does not have permission to list prices for commodities in someone else's entity"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                     (->> (apply-scope {:commodity-id (:id commodity)} :price)
                          (prices/search storage-spec)
                          count)))))))

(deftest price-creation
  (let [context (serialization/realize storage-spec prices-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        commodity (find-commodity context "AAPL")
        price (tag-resource {:commodity-id (:id commodity)
                             :trade-date (t/local-date 2016 3 2)
                             :price 50M}
                            :price)]
    (testing "A user has permission to create a price for a commodity in his own entities"
      (with-authentication john
        (is (allowed? :create price)
            "Create is allowed")))
    (testing "A user does not have permission to create a price for a commodity in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create price))
            "Create is not allowed")))))

(deftest price-management
  (let [context (serialization/realize storage-spec prices-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        price (find-price context 100M (t/local-date 2017 3 2))]
    (testing "A user has permission on prices for commodities in his own entities"
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action price)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on prices for commodities in someone else's entity"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action price))
              (format "A user does not have %s permission" action)))))))

(def grants-context
  (assoc entities-context :grants [{:entity-id "Personal"
                                    :user-id "jane@doe.com"
                                    :permissions {:account #{:index :show}}}]))

(deftest grant-list
  (let [context (serialization/realize storage-spec grants-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (find-entity context "Personal")]
    (testing "A user has permission to list grants in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:entity-id (:id entity)} :grant)
                         (grants/search storage-spec)
                         count))
            "The grants are returned")))
    (testing "A user does not have permission list grants in someone else's entity"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                     (->> (apply-scope{:entity-id (:id entity)} :grant)
                          (grants/search storage-spec)
                          count)))))))

(deftest grant-management
  (let [context (serialization/realize storage-spec grants-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        entity (find-entity context "Personal")
        grant (find-grant context (:id entity) (:id jane))]
    (testing "A user has permission on grants in his own entities"
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action grant)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permission on grants in someone else's entity"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action grant))
              (format "A user does not have %s permission" action)))))))

(deftest grant-creation
  (let [context (serialization/realize storage-spec grants-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        personal (find-entity context "Personal")
        grant (tag-resource {:user-id (:id jane)
                             :entity-id (:id personal)
                             :permissions {:account #{:index :show}}}
                            :grant)]
    (testing "A user has permission to create an grant in his own entities"
      (with-authentication john
        (is (allowed? :create grant)
            "Create is allowed")))
    (testing "A user does not have permission to create an grant in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create grant))
            "Create is not allowed")))))
