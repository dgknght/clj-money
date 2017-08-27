(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.factories.account-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error
                                            simplify-account-groups]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private account-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"}]
   :entities [{:name "Personal"
               :settings {:default-commodity-id "USD"}}]})

(defn- attributes
  [context]
  {:name "Checking"
   :type :asset
   :entity-id (-> context :entities first :id)})

(def ^:private select-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"
                  :entity-id "Personal"}]
   :entities [{:name "Personal"
               :settings {:default-commodity-id "USD"}}]
   :accounts [{:name "Credit card"
               :type :liability}
              {:name "Checking"
               :type :asset}]})

(deftest select-accounts
  (let [context (serialization/realize storage-spec select-context)
        entity-id (-> context :entities first :id)
        actual (->> entity-id
                    (accounts/select-by-entity-id storage-spec)
                    (map #(dissoc % :id
                                    :updated-at
                                    :created-at
                                    :commodity-id)))
        expected [{:name "Checking"
                   :type :asset
                   :balance 0M
                   :entity-id entity-id
                   :commodity {:symbol "USD"
                               :name "US Dollar"
                               :type :currency}}
                  {:name "Credit card"
                   :type :liability
                   :balance 0M
                   :entity-id entity-id
                   :commodity {:symbol "USD"
                               :name "US Dollar"
                               :type :currency}}]]
    (is (= expected actual) "It returns the correct accounts")))

(def ^:private nested-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"
                  :entity-id "Personal"}]
   :entities [{:name "Personal"
               :settings {:default-commodity-id "USD"}}]
   :accounts [{:name "Savings"
               :type :asset}
              {:name "Reserve"
               :type :asset
               :parent-id "Savings"}
              {:name "Car"
               :type :asset
               :parent-id "Savings"}
              {:name "Doug"
               :type :asset
               :parent-id "Car"}
              {:name "Eli"
               :type :asset
               :parent-id "Car"}
              {:name "Checking"
               :type :asset}
              {:name "Taxes"
               :type :expense}
              {:name "Federal Income Tax"
               :type :expense
               :parent-id "Taxes"}
              {:name "Social Security"
               :type :expense
               :parent-id "Taxes"}
              ]})

(deftest select-nested-accounts
  (let [context (serialization/realize storage-spec nested-context)
        entity-id (-> context :entities first :id)
        result (simplify-account-groups
                 (accounts/select-nested-by-entity-id storage-spec entity-id))
        expected [{:type :asset
                   :accounts [{:name "Checking"
                               :path "Checking"}
                              {:name "Savings"
                               :path "Savings"
                               :children [{:name "Car"
                                           :path "Savings/Car"
                                           :children [{:name "Doug"
                                                       :path "Savings/Car/Doug"}
                                                      {:name "Eli"
                                                       :path "Savings/Car/Eli"}]}
                                          {:name "Reserve"
                                           :path "Savings/Reserve"}]}]}
                  {:type :liability
                   :accounts []}
                  {:type :equity
                   :accounts []}
                  {:type :income
                   :accounts []}
                  {:type :expense
                   :accounts [{:name "Taxes"
                               :path "Taxes"
                               :children [{:name "Federal Income Tax"
                                           :path "Taxes/Federal Income Tax"}
                                          {:name "Social Security"
                                           :path "Taxes/Social Security"}]}]}]]
    (when-not (= expected result)
      (pprint {:expected expected
               :actual result
               :diff (diff expected result)}))

    (is (= expected result) "The accounts should be returned in the correct hierarchy")))

(deftest create-an-account
  (let [context (serialization/realize storage-spec account-context)
        result (accounts/create storage-spec (attributes context))
        entity-id (-> context :entities first :id)
        accounts (->> entity-id
                      (accounts/select-by-entity-id storage-spec)
                      (map #(dissoc % :id :updated-at :created-at)))
        expected [{:name "Checking"
                   :type :asset
                   :entity-id entity-id
                   :commodity-id (-> context :commodities first :id)
                   :commodity {:name "US Dollar"
                               :symbol "USD"
                               :type :currency}
                   :balance 0M}]]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors.")
    (is (= expected accounts) "The account can be retrieved")))

(def ^:private duplicate-name-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :name "US Dollar"
                  :type :currency
                  :entity-id "Personal"}
                 {:symbol "USD"
                  :name "US Dollar"
                  :entity-id "Business"
                  :type :currency}]
   :entities [{:name "Personal"
               :settings {:default-commodity-id "USD"}}
              {:name "Business"
               :settings {:default-commodity-id "USD"}}]
   :accounts [{:name "Credit card"
               :type :liability
               :entity-id "Personal"}
              {:name "Auto"
               :type :expense
               :entity-id "Personal"}
              {:name "Repair"
               :type :expense
               :entity-id "Personal"
               :parent-id "Auto"}
              {:name "Household"
               :type :expense
               :entity-id "Personal"} ]})

(deftest duplicate-name-across-entities
  (let [context (serialization/realize storage-spec duplicate-name-context)
        business (first (filter #(= "Business" (:name %)) (:entities context)))
        result (accounts/create storage-spec {:name "Credit card"
                                              :type :liability
                                              :entity-id (:id business)})]
    (is (not (validation/has-error? result))
        "A second account can be created with the same name in a different entity")))

(deftest duplicate-name-across-parents
  (let [context (serialization/realize storage-spec duplicate-name-context)
        business (first (filter #(= "Business" (:name %)) (:entities context)))
        household (first (filter #(= "Household" (:name %)) (:accounts context)))
        result (accounts/create storage-spec {:name "Repair"
                                              :type :expense
                                              :parent-id (:id household)
                                              :entity-id (:id business)})]
    (is (empty? (validation/error-messages result))
        "A name can be dulicated across parents")))

(def ^:private create-child-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"}]
   :entities [{:name "Personal"
               :settings {:default-commodity-id "USD"}}]
   :accounts [{:name "Savings"
               :type :asset}]})

(deftest create-a-child-account
  (let [context (serialization/realize storage-spec create-child-context)
        savings (-> context :accounts first)
        entity (-> context :entities first)
        car (accounts/create storage-spec {:name "Car"
                                           :type :asset
                                           :parent-id (:id savings)
                                           :entity-id (:id entity)})]
    (is (empty? (validation/error-messages car))
        "The model should not have any errors")))

(deftest child-must-have-same-type-as-parent
  (let [context (serialization/realize storage-spec create-child-context)
        savings (-> context :accounts first)
        entity (-> context :entities first)
        result (accounts/create storage-spec {:name "Federal income tax"
                                              :type :expense
                                              :parent-id (:id savings)
                                              :entity-id (:id entity)})]
    (assert-validation-error
      :type
      "Type must match the parent type"
      result)))

(deftest name-is-required
  (let [context (serialization/realize storage-spec account-context)
        attr (-> context
                 attributes
                 (dissoc :name))]
    (assert-validation-error
      :name
      "Name is required"
      (accounts/create storage-spec attr))))

(deftest name-is-unique-within-a-parent
  (let [context (serialization/realize storage-spec duplicate-name-context)
        entity (-> context :entities first)
        auto (first (filter #(= "Auto" (:name %)) (:accounts context)))
        attributes {:name "Repair"
                    :parent-id (:id auto)
                    :entity-id (:id entity)
                    :type :expense}]
    (assert-validation-error
      :name
      "Name is already in use"
      (accounts/create storage-spec attributes))))

(deftest correct-account-type
  (let [context (serialization/realize storage-spec account-context)
        attr (assoc (attributes context) :type :invalidtype)]
    (assert-validation-error
      :type
      "Type must be one of: expense, equity, liability, income, asset"
      (accounts/create storage-spec attr))))

(deftest commodity-id-defaults-to-entity-default
  (let [context (serialization/realize storage-spec account-context)
        commodity (-> context :commodities first)
        account (-> context
                    attributes
                    (dissoc :commodity-id))
        result (accounts/create storage-spec account)]
    (is (= (:id commodity) (:commodity-id result))
        "The specified default commodity is used")))

(deftest update-an-account
  (try
    (let [context (serialization/realize storage-spec select-context)
          account (first (filter #(= "Checking" (:name %)) (:accounts context)))
          result (accounts/update storage-spec (assoc account :name "New name"))
          retrieved (accounts/find-by-id storage-spec (:id account))]
      (is (not (validation/has-error? result))
          (format "Unexpected validation error: %s"
                  (validation/error-messages result)) )
      (is (= "New name" (:name result)) "The updated account is returned")
      (is (= "New name" (:name retrieved)) "The updated account is retreived"))
    (catch clojure.lang.ExceptionInfo e
      (pprint (ex-data e))
      (is false "unexpected validation error"))))

(def same-parent-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Current assets"
               :type :asset}
              {:name "Fixed assets"
               :type :asset}
              {:name "House"
               :type :asset
               :parent-id "Current assets"}]})

(deftest change-an-account-parent
  (let [context (serialization/realize storage-spec same-parent-context)
        [current fixed house] (:accounts context)
        updated (assoc house :parent-id (:id fixed))
        result (accounts/update storage-spec updated)
        retrieved (accounts/reload storage-spec updated)]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (= (:id fixed)
           (:parent-id result))
        "The returned account has the correct parent-id value")
    (is (= (:id fixed)
           (:parent-id retrieved))
        "The retrieved account has the correct parent-id value")) )

(deftest delete-an-account
  (let [context (serialization/realize storage-spec select-context)
        account (-> context :accounts first)
        _ (accounts/delete storage-spec (:id account))
        accounts (accounts/select-by-entity-id storage-spec
                                               (-> context
                                                   :entities
                                                   first
                                                   :id))]
    (is (not-any? #(= (:id account) (:id %)) accounts)
        "The deleted account is no longer returned from the database")))

(defmacro test-amount-polarization
  [context account-type action amount expected message]
  `(let [account# (accounts/create storage-spec (assoc (attributes ~context)
                                                       :type ~account-type))
         item# {:account-id (:id account#)
                :action ~action
                :amount ~amount}
         polarized-amount# (accounts/polarize-amount item# account#)]
     (is (= ~expected polarized-amount#) ~message)))

(deftest polarize-an-amount
  (let [context (serialization/realize storage-spec account-context)]
    ; Debits
    (test-amount-polarization context :asset     :debit 100M  100M "A debit in an asset account increases the balance")
    (test-amount-polarization context :expense   :debit 100M  100M "A debit in an expense account increases the balance")
    (test-amount-polarization context :liability :debit 100M -100M "A debit in an liability account decreases the balance")
    (test-amount-polarization context :equity    :debit 100M -100M "A debit in an equity account decreases the balance")
    (test-amount-polarization context :income    :debit 100M -100M "A debit in an income account decreases the balance")

    ;; Credits
    (test-amount-polarization context :asset     :credit 100M -100M "A credit in an asset account decreases the balance")
    (test-amount-polarization context :expense   :credit 100M -100M "A credit in an expense account dereases the balance")
    (test-amount-polarization context :liability :credit 100M  100M "A credit in an liability account increases the balance")
    (test-amount-polarization context :equity    :credit 100M  100M "A credit in an equity account increases the balance")
    (test-amount-polarization context :income    :credit 100M  100M "A credit in an income account increases the balance")))
