(ns clj-money.permissions.prices-test
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
            [clj-money.models.prices :as prices]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.prices]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-commodity
                                            find-price
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def prices-context
  {:users [(assoc (factory :user) :email "john@doe.com")
           (assoc (factory :user) :email "jane@doe.com")]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}
                 {:name "Apple, Inc."
                  :symbol "AAPL"
                  :type :stock
                  :exchange :nasdaq
                  :entity-id "Personal"}]
   :prices [{:commodity-id "AAPL"
             :trade-date (t/local-date 2017 3 2)
             :price 100M}]})

(deftest price-list
  (let [context (serialization/realize storage-spec prices-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        commodity (find-commodity context "AAPL")
        criteria {:commodity-id (:id commodity)
                  :trade-date [:between
                               (t/local-date 2017 1 1)
                               (t/local-date 2017 12 31)]}]
    (testing "A user has permission to list prices for commodities in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope criteria :price)
                         (prices/search storage-spec)
                         count))
            "The prices are returned")))
    (testing "A user does not have permission to list prices for commodities in someone else's entity"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                     (->> (apply-scope criteria :price)
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
        commodity (find-commodity context "AAPL")
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
              (format "A user does not have %s permission" action)))))
    (testing "A user can be granted permission on prices for commodities in someone else's entity"
      (with-authentication jane
        (grants/create storage-spec {:user-id (:id jane)
                                     :entity-id (:entity-id commodity)
                                     :permissions {:price #{:show}} })
        (doseq [action [:show]]
          (is (allowed? action price)
              (format "A user has %s permission" action)))
        (doseq [action [:edit :update :delete]]
          (is (not (allowed? action price))
              (format "A user does not have %s permission" action)))))))
