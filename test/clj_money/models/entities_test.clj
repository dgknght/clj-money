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
               (select-keys actual [:name :user-id])) "The returned map should have the correct content."))
      (testing "The name can be duplicated between two different users"
        (let [other-entity (entities/create storage-spec
                                            (assoc attributes :user-id (:id other-user)))]
          (is (number? (:id other-entity))))))))

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
           (map #(select-keys % [:name :user-id]) actual)) "The returned list should contain the correct items")
    (is (not-any? #(= "Other entity" (:name %)) actual) "The returned list should not contain other users entities")))

(deftest find-an-entity-by-id
  (let [entity (entities/create storage-spec {:name "Personal"
                                              :user-id (:id user)})
        retrieved (entities/find-by-id storage-spec (:id entity))]
    (is (= entity retrieved))))

(deftest update-an-entity
  (let [entity (entities/create storage-spec {:name "Entity X"
                                              :user-id (:id user)})
        updated (-> entity
                    (assoc :name "Entity Y")
                    (assoc :monitored-account-ids [1 2]))
        result (entities/update storage-spec updated)
        retrieved (entities/find-by-id storage-spec (:id entity))
        expected {:id (:id entity)
                  :name "Entity Y"
                  :user-id (:id user)
                  :monitored-account-ids [1 2]}
        actual (select-keys retrieved [:id
                                       :name
                                       :user-id
                                       :monitored-account-ids])]

    (if (validation/has-error? result)
      (pprint {:result result}))

    (is (= "Entity Y" (:name result)) "The result contains the correct values")

    (if-not (= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))

    (is (= expected actual) "The retreived value has the correct values")))

(deftest delete-an-entity
  (let [entity (entities/create storage-spec {:name "Entity X"
                                              :user-id (:id user)})
        _ (entities/delete storage-spec (:id entity))
        retrieved (entities/find-by-id storage-spec (:id entity))]
    (is (nil? retrieved) "The entity is not returned after delete")))

(deftest inventory-method-defaults-to-fifo
  (let [entity (entities/create storage-spec {:name "Personal"
                                              :user-id (:id user)})]
    (is (= :fifo (:inventory-method entity)) "The default value is correct")))

(deftest inventory-method-can-be-lifo
  (let [entity (entities/create storage-spec {:name "Personal"
                                              :user-id (:id user)
                                              :inventory-method :lifo})]
    (is (empty? (validation/error-messages entity)) "The entity is valid")))

(deftest inventory-method-cannot-be-something-other-than-fifo-or-lifo
  (let [entity (entities/create storage-spec {:name "Personal"
                                              :user-id (:id user)
                                              :inventory-method :not-valid})]
    (is (= ["Inventory method must be one of: fifo, lifo"]
           (validation/error-messages entity :inventory-method))
        "There is an error message for the attributes")))
