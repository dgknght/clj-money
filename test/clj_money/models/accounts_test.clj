(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.factories.account-factory]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error
                                            simplify-account-groups]]))

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

(deftest select-nested-accounts
  (let [savings (accounts/create storage-spec {:name "Savings"
                                               :type :asset
                                               :entity-id (:id entity)})
        reserve-savings (accounts/create storage-spec {:name "Reserve"
                                                       :type :asset
                                                       :parent-id (:id savings)
                                                       :entity-id (:id entity)})
        car-savings (accounts/create storage-spec {:name "Car"
                                                   :type :asset
                                                   :parent-id (:id savings)
                                                   :entity-id (:id entity)})
        doug-car (accounts/create storage-spec {:name "Doug"
                                                   :type :asset
                                                   :parent-id (:id car-savings)
                                                   :entity-id (:id entity)})
        eli-car (accounts/create storage-spec {:name "Eli"
                                                   :type :asset
                                                   :parent-id (:id car-savings)
                                                   :entity-id (:id entity)})
        checking (accounts/create storage-spec {:name "Checking"
                                                :type :asset
                                                :entity-id (:id entity)})
        taxes (accounts/create storage-spec {:name "Taxes"
                                             :type :expense
                                             :entity-id (:id entity)})
        fit (accounts/create storage-spec {:name "Federal Income Tax"
                                           :type :expense
                                           :parent-id (:id taxes)
                                           :entity-id (:id entity)})
        ss (accounts/create storage-spec {:name "Social Security"
                                          :type :expense
                                          :parent-id (:id taxes)
                                          :entity-id (:id entity)})
        result (simplify-account-groups
                 (accounts/select-nested-by-entity-id storage-spec (:id entity)))
        expected [{:type :asset
                   :accounts [{:name "Checking"}
                              {:name "Savings"
                               :children [{:name "Car"
                                           :children [{:name "Doug"}
                                                      {:name "Eli"}]}
                                          {:name "Reserve"}]}]}
                  {:type :liability
                   :accounts []}
                  {:type :equity
                   :accounts []}
                  {:type :income
                   :accounts []}
                  {:type :expense
                   :accounts [{:name "Taxes"
                               :children [{:name "Federal Income Tax"}
                                          {:name "Social Security"}]}]}]]
    (when-not (= expected result)
      (pprint {:expected expected
               :actual result
               :diff (diff expected result)}))

    (is (= expected result) "The accounts should be returned in the correct hierarchy")))

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create storage-spec attributes)
    (let [accounts (map #(select-keys % [:name :type])
                        (accounts/select-by-entity-id storage-spec
                                                      (:id entity)))
          expected [{:name "Checking"
                     :type :asset}]]
      (is (= expected
             accounts)))))

(deftest values-are-coerced-on-create
  (try
    (let [result (accounts/create storage-spec
                                  (-> attributes
                                      (update-in [:entity-id] str)
                                      (assoc :name "Coerced")))]
      (is (number? (:id result))))
    (catch clojure.lang.ExceptionInfo e
      (pprint (ex-data e))
      (is false "Unexpected validation error"))))

(deftest duplicate-name-across-entities
  (let [other-entity (entities/create storage-spec {:name "My other life"
                                                    :user-id (:id user)})
        a1 (accounts/create storage-spec {:name "Credit card"
                                          :type :liability
                                          :entity-id (:id other-entity)})
        a2 (accounts/create storage-spec {:name "Credit card"
                                          :type :liability
                                          :entity-id (:id entity)})]
    (is (not (validation/has-error? a2)) "A second account can be created with the same name in a different entity")))

(deftest duplicate-name-across-parents
  (let [auto (accounts/create storage-spec {:name "Auto"
                                            :type :expense
                                            :entity-id (:id entity)})
        auto-repair (accounts/create storage-spec {:name "Repair"
                                                   :type :expense
                                                   :parent-id (:id auto)
                                                   :entity-id (:id entity)})
        household (accounts/create storage-spec {:name "Household"
                                                 :type :expense
                                                 :entity-id (:id entity)})
        household-repair (accounts/create storage-spec {:name "Repair"
                                                        :type :expense
                                                        :parent-id (:id household)
                                                        :entity-id (:id entity)})]
    (is (validation/valid? household-repair) "A name can be dulicated across parents")))

(deftest create-a-child-account
  (let [savings (accounts/create storage-spec {:name "Savings"
                                               :type :asset
                                               :entity-id (:id entity)})
        car (accounts/create storage-spec {:name "Car"
                                           :type :asset
                                           :parent-id (:id savings)
                                           :entity-id (:id entity)})]
    (is (validation/valid? car) "The model should not have any errors")))

(deftest child-must-have-same-type-as-parent
  (let [savings (accounts/create storage-spec {:name "Savings"
                                               :type :asset
                                               :entity-id (:id entity)})
        fit (accounts/create storage-spec {:name "Federal income tax"
                                           :type :expense
                                           :parent-id (:id savings)
                                           :entity-id (:id entity)})]
    (is (= ["Type must match the parent type"]
           (validation/get-errors fit :type)) "The model should not be valid")))

(deftest name-is-required
  (assert-validation-error
      :name
      "Name is required"
      (accounts/create storage-spec (dissoc attributes :name))))

(deftest name-is-unique-within-a-parent
  (accounts/create storage-spec attributes)
  (assert-validation-error
    :name
    "Name is already in use"
    (accounts/create storage-spec attributes)))

(deftest correct-account-type
  (assert-validation-error
    :type
    "Type must be one of: expense, equity, liability, income, asset"
    (accounts/create storage-spec (assoc attributes :type :invalidtype))))

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

(defmacro test-amount-polarization
  [account-type action amount expected message]
  `(let [account# (accounts/create storage-spec (merge (factory :account)
                                                       {:type ~account-type
                                                        :entity-id (:id entity)}))
         item# {:account-id (:id account#)
                :action ~action
                :amount ~amount}
         polarized-amount# (accounts/polarize-amount item# account#)]
     (is (= ~expected polarized-amount#) ~message)))

(deftest polarize-an-amount
  ; Debits
  (test-amount-polarization :asset     :debit (bigdec 100) (bigdec  100) "A debit in an asset account increases the balance")
  (test-amount-polarization :expense   :debit (bigdec 100) (bigdec  100) "A debit in an expense account increases the balance")
  (test-amount-polarization :liability :debit (bigdec 100) (bigdec -100) "A debit in an liability account decreases the balance")
  (test-amount-polarization :equity    :debit (bigdec 100) (bigdec -100) "A debit in an equity account decreases the balance")
  (test-amount-polarization :income    :debit (bigdec 100) (bigdec -100) "A debit in an income account decreases the balance")

  ;; Credits
  (test-amount-polarization :asset     :credit (bigdec 100) (bigdec -100) "A credit in an asset account decreases the balance")
  (test-amount-polarization :expense   :credit (bigdec 100) (bigdec -100) "A credit in an expense account dereases the balance")
  (test-amount-polarization :liability :credit (bigdec 100) (bigdec  100) "A credit in an liability account increases the balance")
  (test-amount-polarization :equity    :credit (bigdec 100) (bigdec  100) "A credit in an equity account increases the balance")
  (test-amount-polarization :income    :credit (bigdec 100) (bigdec  100) "A credit in an income account increases the balance"))
