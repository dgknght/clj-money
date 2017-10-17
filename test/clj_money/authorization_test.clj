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
