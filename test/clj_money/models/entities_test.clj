(ns clj-money.models.entities-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-entity]]
            [clj-money.models.entities :as entities]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def ^:private entity-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]})

(defn- attributes
  [context]
  {:name "Personal"
   :user-id (-> context :users first :id)})

(deftest create-an-entity
  (testing "An entity can be created with valid attributes"
    (let [context (realize entity-context)
          [user other-user] (:users context)
          actual (entities/put (attributes context))
          expected {:name "Personal"
                    :user-id (:id user)}]
      (testing "The new entity is returned"
        (is (= expected
               (select-keys actual [:name :user-id]))
            "The returned map should have the correct content."))
      (testing "The name can be duplicated between two different users"
        (let [other-entity (entities/put (assoc (attributes context) :user-id (:id other-user)))]
          (is (number? (:id other-entity))))))))

(deftest attempt-to-create-an-invalid-entity
  (let [context (realize entity-context)]
    (testing "Name is required"
      (is (thrown-with-ex-data?
            "Validation failed"
            {::v/errors {:entity/name ["Name is required"]}}
            (entities/put (dissoc (attributes context) :name)))))
    (testing "Name must be unique"
      (entities/put (attributes context))
      (is (thrown-with-ex-data?
            "Validation failed"
            {::v/errors {:entity/name ["Name is already in use"]}}
            (entities/put (attributes context)))))))

(def ^:private list-context
  (-> entity-context
      (assoc :entities [{:name "Personal"
                         :user-id "john@doe.com"}
                        {:name "Business"
                         :user-id "john@doe.com"}])))

(deftest get-a-list-of-entities
  (let [context (realize list-context)
        john (find-user context "john@doe.com")
        actual (entities/select {:user-id (:id john)})]
    (is (= #{"Personal" "Business"} (->> actual
                                         (map :name)
                                         set))
        "The correct entities are returned")))

(deftest find-an-entity-by-id
  (let [ctx (realize list-context)
        entity (find-entity ctx "Personal")
        retrieved (entities/find (:id entity))]
    (is (= entity retrieved))))

(deftest update-an-entity
  (let [context (realize list-context)
        user (-> context :users first)
        entity (find-entity context "Personal")
        updated (-> entity
                    (assoc :name "Entity Y")
                    (assoc-in [:settings :monitored-account-ids] #{1 2}))
        result (entities/update updated)
        retrieved (entities/find entity)
        expected {:id (:id entity)
                  :name "Entity Y"
                  :user-id (:id user)
                  :settings {:monitored-account-ids #{1 2}}}
        actual (dissoc retrieved :updated-at :created-at)]
    (is (= "Entity Y" (:name result)) "The result contains the correct values")
    (is (= expected actual) "The retreived value has the correct values")))

(deftest delete-an-entity
  (let [context (realize entity-context)
        entity (entities/put (attributes context))
        _ (entities/delete entity)
        retrieved (entities/find entity)]
    (is (nil? retrieved) "The entity is not returned after delete")))

(deftest inventory-method-can-be-lifo
  (let [context (realize entity-context)]
    (is (valid? (entities/put (-> context
                                     attributes
                                     (assoc :settings {:inventory-method :lifo})))))))

(deftest inventory-method-cannot-be-something-other-than-fifo-or-lifo
  (let [context (realize entity-context)]
    (is (invalid? (entities/put (-> context
                                       attributes
                                       (assoc :settings {:inventory-method :not-valid})))
                  [:settings :inventory-method]
                  "Inventory method must be fifo or lifo"))))
