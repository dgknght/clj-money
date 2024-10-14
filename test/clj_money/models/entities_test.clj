(ns clj-money.models.entities-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-entity]]
            [clj-factory.core :refer [factory]]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-updated]]
            [clj-money.models :as models]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def ^:private entity-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})])

(def ^:private list-context
  (conj entity-context
        #:entity{:name "Personal"
                 :user "john@doe.com"}
        #:entity{:name "Business"
                 :user "john@doe.com"}))

(defn- attributes []
  #:entity{:name "Personal"
           :user (find-user "john@doe.com")})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:entity/user]))

(deftest create-an-entity
  (with-context entity-context
    (testing "An entity can be created with minimal attributes"
      (assert-created (attributes)))
    (testing "An entity can be created with all attributes"
      (assert-created
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
    (assert-invalid (dissoc (attributes) :entity/name)
                    {:entity/name ["Name is required"]})))

(deftest name-is-unique-for-a-user
  (with-context list-context
    (assert-invalid (attributes)
                    {:entity/name ["Name is already in use"]})))

(deftest name-can-be-duplicated-between-users
  (with-context list-context
    (let [user (find-user "jane@doe.com") ]
      (assert-created (assoc (attributes)
                             :entity/user user)))))

(deftest get-a-list-of-entities
  (with-context list-context
    (let [user (find-user "john@doe.com")]
      (is (seq-of-maps-like? [{:entity/name "Business"}
                              {:entity/name "Personal"}]
                             (models/select {:entity/user user}
                                            {:order-by [[:entity/name :asc]]}))
          "Entities matching the criteria are returned"))))

(deftest update-an-entity
  (with-context list-context
    (assert-updated (find-entity "Personal")
                    #:entity{:name "Entity Y"
                             :settings {:settings/monitored-account-ids #{1 2}}})))

(deftest delete-an-entity
  (with-context list-context
    (let [entity (find-entity "Personal")]
      (models/delete entity)
      (is (nil? (models/find entity))
          "The entity is not returned after delete"))))

(deftest inventory-method-can-be-lifo
  (with-context entity-context
    (is (comparable? {:entity/settings {:settings/inventory-method :lifo}}
                     (models/put (assoc (attributes)
                                        :entity/settings {:settings/inventory-method :lifo}))))))

(deftest inventory-method-cannot-be-something-other-than-fifo-or-lifo
  (with-context entity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:entity/settings
                       {:settings/inventory-method
                        ["Inventory method must be fifo or lifo"]}}}
          (models/put (assoc (attributes)
                             :entity/settings {:settings/inventory-method :not-valid}))))))
