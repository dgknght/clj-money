(ns clj-money.models.entities-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-entity]]
            [clj-money.models.entities :as entities]
            [clj-factory.core :refer [factory]]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def ^:private entity-context
  {:users [(factory :user {:user/email "john@doe.com"})
           (factory :user {:user/email "jane@doe.com"})]})

(def ^:private list-context
  (assoc entity-context
         :entities [#:entity{:name "Personal"
                             :user "john@doe.com"}
                    #:entity{:name "Business"
                             :user "john@doe.com"}]))

(defn- attributes []
  #:entity{:name "Personal"
           :user (find-user "john@doe.com")})

(defn- test-entity-creation
  [attr]
  (let [created (entities/put attr)
        expected (update-in attr [:entity/user] select-keys [:id])]
    (is (:id created)
        "The return value has an :id attribute")
    (is (comparable? expected created)
        "The returned value has the specified attributes")
    (is (comparable? expected (entities/find created))
        "A retrieved value has the specified attributes")))

(deftest create-an-entity
  (with-context entity-context
    (testing "An entity can be created with minimal attributes"
      (test-entity-creation (attributes)))
    (testing "An entity can be created with all attributes"
      (test-entity-creation
        #:entity{:name "Business"
                 :user (find-user "john@doe.com")
                 :settings #:settings{:inventory-method :fifo
                                      :monitored-account-ids #{1 2 3}
                                      :earliest-transaction-date (t/local-date 2020 1 1)
                                      :latest-transaction-date (t/local-date 2020 12 31)
                                      :earliest-price-date (t/local-date 2020 1 1)
                                      :latest-price-date (t/local-date 2020 12 31)}}))))

(deftest name-is-required
  (with-context entity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:entity/name ["Name is required"]}}
          (entities/put (dissoc (attributes) :entity/name))))))

(deftest name-is-unique-for-a-user
  (with-context list-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:entity/name ["Name is already in use"]}}
          (entities/put (attributes))))))

(deftest name-can-be-duplicated-between-users
  (with-context list-context
    (let [user (find-user "jane@doe.com")
          attr (assoc (attributes)
                      :entity/user user)]
      (is (comparable? (assoc attr
                              :entity/user {:id (:id user)})
                       (entities/put attr))))))

(deftest get-a-list-of-entities
  (with-context list-context
    (let [user (find-user "john@doe.com")]
      (is (seq-of-maps-like? [{:entity/name "Business"}
                              {:entity/name "Personal"}]
                             (entities/select {:entity/user user}
                                              {:order-by [[:entity/name :asc]]}))
          "Entities matching the criteria are returned"))))

(deftest update-an-entity
  (with-context list-context
    (let [entity (find-entity "Personal")
          updates #:entity{:name "Entity Y"
                           :settings {:settings/monitored-account-ids #{1 2}}}]
      (is (comparable? updates (entities/put (merge entity updates)))
          "The return value contains the updated attributes")
      (is (comparable? updates (entities/find entity))
          "The retrieved value has the updated attributes"))))

(deftest delete-an-entity
  (with-context list-context
    (let [entity (find-entity "Personal")]
      (entities/delete entity)
      (is (nil? (entities/find entity))
          "The entity is not returned after delete"))))

(deftest inventory-method-can-be-lifo
  (with-context entity-context
    (is (comparable? {:entity/settings {:settings/inventory-method :lifo}}
                     (entities/put (assoc (attributes)
                                          :entity/settings {:settings/inventory-method :lifo}))))))

(deftest inventory-method-cannot-be-something-other-than-fifo-or-lifo
  (with-context entity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:entity/settings
                       {:settings/inventory-method
                        ["Inventory method must be fifo or lifo"]}}}
          (entities/put (assoc (attributes)
                               :entity/settings {:settings/inventory-method :not-valid}))))))
