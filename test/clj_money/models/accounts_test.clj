(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.jdbc :as jdbc]
            [clj-factory.core :refer [factory]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.test-helpers :refer [reset-db
                                            assert-throws-validation-exception]]))

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
        (is false "Unexpected validation error")))))

(deftest attempt-to-create-an-invalid-account
  (assert-throws-validation-exception
    {:name 'missing-required-key}
    (accounts/create storage-spec (dissoc attributes :name))))
