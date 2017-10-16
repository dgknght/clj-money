(ns clj-money.permissions.grants-test
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
            [clj-money.permissions.grants]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-grant
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def grants-context
  {:users [(assoc (factory :user) :email "john@doe.com")
           (assoc (factory :user) :email "jane@doe.com")]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}]
   :grants [{:entity-id "Personal"
             :user-id "jane@doe.com"
             :permissions {:account #{:index :show}}}]})

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
