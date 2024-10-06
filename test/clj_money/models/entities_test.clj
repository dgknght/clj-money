(ns clj-money.models.entities-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
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

(deftest create-an-entity
  (with-context entity-context
    (testing "An entity can be created with valid attributes"
      (let [attr (attributes)
            created (entities/put attr)
            expected (update-in attr [:entity/user] select-keys [:id])]
        (is (:id created)
            "The return value has an :id attribute")
        (is (comparable? expected created)
            "The returned value has the specified attributes")
        (is (comparable? expected (entities/find created))
            "A retrieved value has the specified attributes")))))

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
    (let [attr (assoc (attributes)
                      :entity/user (find-user "jane@doe.com"))]
      (is (comparable? attr (entities/put attr))))))

(deftest get-a-list-of-entities
  (with-context list-context
    (let [user (find-user "john@doe.com")]
      (is (seq-of-maps-like? [{:entity/name "Business"}
                              {:entity/name "Personal"}]
                             (entities/select {:user user}
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
    (is (comparable? {:settings {:inventory-method :lifo}}
                     (entities/put (assoc (attributes)
                                          :settings {:inventory-method :lifo}))))))

(deftest inventory-method-cannot-be-something-other-than-fifo-or-lifo
  (with-context entity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:entity/settings ["Inventory method must be fifo or lifo"]}}
          (entities/put (assoc (attributes)
                               :settings {:inventory-method :not-valid}))))))
