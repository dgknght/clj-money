(ns clj-money.models.entities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-money.validation :as validation]
            [clj-money.serialization :as serialization]
            [clj-money.models.entities :as entities]
            [clj-money.models.users :as users]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory])
  (:use [clj-money.test-helpers :refer :all]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private entity-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]})

(defn- attributes
  [context]
  {:name "Personal"
   :user-id (-> context :users first :id)})

(deftest create-an-entity
  (testing "An entity can be created with valid attributes"
    (let [context (serialization/realize storage-spec entity-context)
          [user other-user] (:users context)
          actual (entities/create storage-spec (attributes context))
          expected {:name "Personal"
                    :user-id (:id user)}]
      (testing "The new entity is returned"
        (is (= expected
               (select-keys actual [:name :user-id]))
            "The returned map should have the correct content."))
      (testing "The name can be duplicated between two different users"
        (let [other-entity (entities/create storage-spec
                                            (assoc (attributes context) :user-id (:id other-user)))]
          (is (number? (:id other-entity))))))))

(deftest attempt-to-create-an-invalid-entity
  (let [context (serialization/realize storage-spec entity-context)
        user (-> context :users first)]
    (testing "Name is required"
      (assert-validation-error
        :name
        "Name is required"
        (entities/create storage-spec (dissoc (attributes context) :name))))
    (testing "Name must be unique"
      (entities/create storage-spec (attributes context))
      (assert-validation-error
        :name
        "Name is already in use"
        (entities/create storage-spec (attributes context))))))

(deftest select-entities-for-a-user
  (let [context (serialization/realize storage-spec entity-context)
        [user other-user] (:users context)
        other-entity (entities/create storage-spec {:name "Other entity"
                                                    :user-id (:id other-user)})
        _ (mapv #(entities/create storage-spec {:name %
                                                :user-id (:id user)})
                ["Personal"
                 "Business"])
        actual (entities/select storage-spec (:id user))
        expected [{:name "Business"
                   :user-id (:id user)}
                  {:name "Personal"
                   :user-id (:id user)}]]
    (is (= expected
           (map #(select-keys % [:name :user-id]) actual)) "The returned list should contain the correct items")
    (is (not-any? #(= "Other entity" (:name %)) actual) "The returned list should not contain other users entities")))

(def ^:private grants-context
  (-> entity-context
      (assoc :entities [{:name "Personal"
                         :user-id "john@doe.com"}
                        {:name "Business"
                         :user-id "jane@doe.com"}])
      (assoc :grants [{:entity-id "Business"
                       :user-id "john@doe.com"
                       :permissions {:account #{:index}}}])))

(deftest select-owned-and-granted-entities-for-a-user
  (let [context (serialization/realize storage-spec grants-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        expected #{{:name "Personal"
                    :user-id (:id john)}
                   {:name "Business"
                    :user-id (:id jane)}}
        actual (->> (entities/select storage-spec (:id john) {:include-grants? true})
                    (map #(dissoc % :created-at :updated-at :id :settings))
                    (into #{}))]
    (if-not (= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))
    (is (= expected actual)
        "The correct entities are returned")))

(deftest find-an-entity-by-id
  (let [context (serialization/realize storage-spec entity-context)
        entity (entities/create storage-spec (attributes context))
        retrieved (entities/find-by-id storage-spec (:id entity))]
    (is (= entity retrieved))))

(deftest update-an-entity
  (let [context (serialization/realize storage-spec entity-context)
        user (-> context :users first)
        entity (entities/create storage-spec {:name "Entity X"
                                              :user-id (:id user)})
        updated (-> entity
                    (assoc :name "Entity Y")
                    (assoc-in [:settings :monitored-account-ids] [1 2]))
        result (entities/update storage-spec updated)
        retrieved (entities/find-by-id storage-spec (:id entity))
        expected {:id (:id entity)
                  :name "Entity Y"
                  :user-id (:id user)
                  :settings {:monitored-account-ids [1 2]}}
        actual (dissoc retrieved :updated-at :created-at)]

    (if (validation/has-error? result)
      (pprint {:result result}))

    (is (= "Entity Y" (:name result)) "The result contains the correct values")

    (if-not (= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))

    (is (= expected actual) "The retreived value has the correct values")))

(deftest delete-an-entity
  (let [context (serialization/realize storage-spec entity-context)
        entity (entities/create storage-spec (attributes context))
        _ (entities/delete storage-spec (:id entity))
        retrieved (entities/find-by-id storage-spec (:id entity))]
    (is (nil? retrieved) "The entity is not returned after delete")))

(deftest inventory-method-can-be-lifo
  (let [context (serialization/realize storage-spec entity-context)
        entity (entities/create storage-spec
                                (-> context
                                    attributes
                                    (assoc :settings {:inventory-method :lifo})))]
    (is (empty? (validation/error-messages entity)) "The entity is valid")))

(deftest inventory-method-cannot-be-something-other-than-fifo-or-lifo
  (let [context (serialization/realize storage-spec entity-context)
        entity (entities/create storage-spec
                                (-> context
                                    attributes
                                    (assoc :settings {:inventory-method :not-valid})))]
    (is (= {:inventory-method ["Inventory method must be one of: fifo, lifo"]}
           (validation/error-messages entity :settings))
        "There is an error message for the attributes")))
