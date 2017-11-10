(ns clj-money.permissions.entities-test
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
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.entities]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-commodity
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
