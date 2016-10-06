(ns clj-money.models.transactions-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def user (users/create storage-spec (factory :user)))
(def entity (entities/create storage-spec
                             (assoc (factory :entity) :user-id (:id user))))

(def account-defs
  [{:name "Checking"
    :type :asset}
   {:name "Salary"
    :type :income}
   {:name "Groceries"
    :type :expense}])

(def accounts
  (zipmap [:checking :salary :groceries]
          (->> account-defs
               (map #(assoc % :entity-id (:id entity)))
               (map #(accounts/create storage-spec %)))))

(def attributes
  {:transaction-date (t/local-date 2016 3 2)
   :entity-id (:id entity)
   :items [{:account-id (-> accounts :checking :id)
            :action :debit
            :amount (bigdec 1000)}
           {:account-id (-> accounts :salary :id)
            :action :credit
            :amount (bigdec 1000)}]})

(deftest create-a-transaction
  (let [transaction (transactions/create storage-spec attributes)]
    (testing "return value includes the new id"
      (is (validation/valid? transaction))
      (is (number? (:id transaction)) "A map with the new ID is returned"))
    (testing "transaction can be retrieved"
      (let [retrieved (transactions/find-by-id storage-spec (:id transaction))]
        (is retrieved "The transaction is retrievable by ID")
        (is (= 2
               (count (:items retrieved))) "The items are returned with the transaction")))))

(deftest transaction-date-is-required
  (let [transaction (transactions/create storage-spec (dissoc attributes :transaction-date))]
    (is (validation/has-error? transaction :transaction-date))))

(deftest entity-id-is-required
  (let [transaction (transactions/create storage-spec (dissoc attributes :entity-id))]
    (is (validation/has-error? transaction :entity-id))))

(deftest item-account-id-is-required
  (let [transaction (transactions/create storage-spec (update-in attributes [:items 0] #(dissoc % :account-id)))]

    (pprint {:transaction transaction})

    (is (validation/has-error? transaction))))

;; TODO
; item account-id is required
; item amount is required
; item amount must be greater than zero
; item action is required
; item action is :debit or :credit
; sub of debits must equal sum of credits
