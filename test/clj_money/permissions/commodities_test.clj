(ns clj-money.permissions.commodities-test
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
            [clj-money.models.commodities :as commodities]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.commodities]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-commodity
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def commodities-context
  {:users [(assoc (factory :user) :email "john@doe.com")
           (assoc (factory :user) :email "jane@doe.com")]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}]})

(deftest commodity-list
  (let [context (serialization/realize storage-spec commodities-context)
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
  (let [context (serialization/realize storage-spec commodities-context)
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
  (let [context (serialization/realize storage-spec commodities-context)
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
