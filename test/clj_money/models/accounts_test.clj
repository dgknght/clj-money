(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.jdbc :as jdbc]
            [clj-factory.core :refer [factory]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def user (users/create storage-spec (factory :user)))
(def entity (entities/create storage-spec
                             (assoc (factory :entity) :user-id (:id user))))

(def attributes
  {:name "Checking"
   :type :asset
   :entity-id (:id entity)})

(deftest select-accounts
  (let [a1 (accounts/create storage-spec attributes)
        a2 (accounts/create storage-spec {:name "Credit card"
                                          :type :liability
                                          :entity-id (:id entity)})
        actual (map #(select-keys % [:name :type])
                    (accounts/select-by-entity-id storage-spec (:id entity)))
        expected [{:name "Checking"
                   :type :asset}
                  {:name "Credit card"
                   :type :liability}]]
    (is (= expected actual) "It returns the correct accounts")))

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create storage-spec attributes)
    (let [accounts (map #(select-keys % [:name :type])
                        (accounts/select-by-entity-id storage-spec
                                                      (:id entity)))
          expected [{:name "Checking"
                     :type :asset}]]
      (is (= expected
             accounts))))
  (testing "Values are coerced into the correct types"
    (try
    (let [result (accounts/create storage-spec
                                  (-> attributes
                                      (update-in [:entity-id] str)
                                      (assoc :name "Coerced")))]
      (is (number? (:id result))))
      (catch clojure.lang.ExceptionInfo e
        (pprint (ex-data e))
        (is false "Unexpected validation error"))))
  (testing "Name can be duplicated across entities"
    (let [other-entity (entities/create storage-spec {:name "My other life"
                                                      :user-id (:id user)})
          a1 (accounts/create storage-spec {:name "Credit card"
                                            :type :liability
                                            :entity-id (:id other-entity)})
          a2 (accounts/create storage-spec {:name "Credit card"
                                            :type :liability
                                            :entity-id (:id entity)})]
      (is (not (validation/has-error? a2)) "A second account can be created with the same name in a different entity"))))

(deftest create-a-child-account
  (let [savings (accounts/create storage-spec {:name "Savings"
                                               :type :asset
                                               :entity-id (:id entity)})
        car (accounts/create storage-spec {:name "Car"
                                           :type :asset
                                           :parent-id (:id savings)
                                           :entity-id (:id entity)})]
    (is (validation/valid? car) "The model should not have any errors")))

(deftest attempt-to-create-an-invalid-account
  (testing "name is required"
    (assert-validation-error
      :name
      "Name is required"
      (accounts/create storage-spec (dissoc attributes :name))))
  (testing "name is unique within an entity"
    (accounts/create storage-spec attributes)
    (assert-validation-error
      :name
      "Name is already in use"
      (accounts/create storage-spec attributes)))
  (testing "type must be asset, liability, equity, income or expense"
    (assert-validation-error
      :type
      "Type must be one of: expense, equity, liability, income, asset"
      (accounts/create storage-spec (assoc attributes :type :invalidtype)))))

(deftest update-an-account
  (try
    (let [account (accounts/create storage-spec attributes)
          updated (accounts/update storage-spec (assoc account :name "New name"))]
      (is (not (validation/has-error? updated))
          (format "Unexpected validation error: %s"
                  (validation/get-errors updated)) )
      (is (= "New name" (:name updated)) "The updated account is returned"))
    (catch clojure.lang.ExceptionInfo e
      (pprint (ex-data e))
      (is false "unexpected validation error"))))

(deftest change-an-account-parent
  (let [current-assets (accounts/create storage-spec {:name "Current assets"
                                                      :type :asset
                                                      :entity-id (:id entity)})
        fixed-assets (accounts/create storage-spec {:name "Fixed assets"
                                                    :type :asset
                                                    :entity-id (:id entity)})
        house (accounts/create storage-spec {:name "House"
                                             :type :asset
                                             :parent-id (:id current-assets)
                                             :entity-id (:id entity)})
        updated (accounts/update storage-spec {:id (:id house)
                                               :parent-id (:id fixed-assets)
                                               :entity-id (:entity-id house)})]
    (is (validation/valid? updated) "The account has no validation errors")
    (is (= (:id fixed-assets)
           (:parent-id updated)) "The returned account has the correct parent-id value")) )

(deftest delete-an-account
  (let [account (accounts/create storage-spec attributes)
        _ (accounts/delete storage-spec (:id account))
        accounts (accounts/select-by-entity-id storage-spec (:id entity))]
    (is (not-any? #(= (:id account) (:id %)) accounts) "The deleted account is no longer returned from the database")))
