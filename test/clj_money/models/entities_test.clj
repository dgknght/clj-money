(ns clj-money.models.entities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]
            [clj-money.models.users :as users]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory])
  (:use [clj-money.test-helpers :refer :all]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def user (users/create storage-spec (factory :user)))
(def attributes {:name "Personal"
                 :user-id (:id user)})
(def other-user (users/create storage-spec (factory :user)))

(deftest create-an-entity
  (testing "An entity can be created with valid attributes"
    (let [actual (entities/create storage-spec attributes)
          expected {:name "Personal"
                    :user-id (:id user)}]
      (testing "The new entity is returned"
        (is (= expected
               (dissoc actual :id)) "The returned map should have the correct content."))
      (testing "The name can be duplicated between two different users"
        (let [other-entity (entities/create storage-spec
                                            (assoc attributes :user-id (:id other-user)))]
          (is (number? (:id other-entity)))))))
  (testing "Attributes are coerced into the correct types"
    (let [result (entities/create storage-spec {:user-id (str (:id user))
                                                :name "Coerced"})]
      (is (number? (:id result))))))

(deftest attempt-to-create-an-invalid-entity
  (testing "Name is required"
    (assert-validation-error
      :name
      "Name is required"
      (entities/create storage-spec (dissoc attributes :name))))
  (testing "Name must be unique"
    (entities/create storage-spec attributes)
    (assert-validation-error
      :name
      "Name is already in use"
      (entities/create storage-spec attributes))))

(deftest select-entities-for-a-user
  (let [other-entity (entities/create storage-spec {:name "Other entity"
                                                    :user-id (:id other-user)})
        _ (dorun (map #(entities/create storage-spec {:name %
                                                      :user-id (:id user)})
                      ["Personal"
                       "Business"]))
        actual (entities/select storage-spec (:id user))
        expected [{:name "Business"
                   :user-id (:id user)}
                  {:name "Personal"
                   :user-id (:id user)}]]
    (is (= expected
           (map #(dissoc % :id) actual)) "The returned list should contain the correct items")
    (is (not-any? #(= "Other entity" (:name %)) actual) "The returned list should not contain other users entities")))

(deftest find-an-entity-by-id
  (let [entity (entities/create storage-spec {:name "Personal"
                                              :user-id (:id user)})
        retrieved (entities/find-by-id storage-spec (:id entity))]
    (is (= entity retrieved))))

(deftest update-an-entity
  (let [entity (entities/create storage-spec {:name "Entity X"
                                              :user-id (:id user)})
        _ (entities/update storage-spec {:id (:id entity)
                                                :name "Entity Y"})
        retrieved (entities/find-by-id storage-spec (:id entity))]
    (is (= {:id (:id entity)
            :name "Entity Y"
            :user-id (:id user)}
           retrieved))))

(deftest delete-an-entity
  (let [entity (entities/create storage-spec {:name "Entity X"
                                              :user-id (:id user)})
        _ (entities/delete storage-spec (:id entity))
        retrieved (entities/find-by-id storage-spec (:id entity))]
    (is (nil? retrieved) "The entity is not returned after delete")))
