(ns clj-money.models.accounts-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.factories.account-factory]
            [clj-money.test-context :refer [realize
                                            find-entity
                                            find-commodity
                                            find-account]]
            [clj-money.validation :as v]
            [clj-money.models.accounts :as accounts]
            [clj-money.accounts :refer [nest
                                        polarize-quantity
                                        derive-action]]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff
                                            assert-validation-error
                                            simplify-account-groups]]))

(use-fixtures :each reset-db)

(def ^:private account-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"}]
   :entities [{:name "Personal"}]})

(defn- attributes
  [context]
  {:name "Checking"
   :type :asset
   :entity-id (-> context :entities first :id)
   :tags #{:something-special}})

(def ^:private select-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"
                  :entity-id "Personal"}]
   :entities [{:name "Personal"}]
   :accounts [{:name "Credit card"
               :type :liability}
              {:name "Checking"
               :type :asset}]})

(deftest select-accounts
  (let [context (realize select-context)
        entity-id (-> context :entities first :id)
        actual (->> (accounts/search {:entity-id entity-id})
                    (map #(select-keys % [:name
                                          :type
                                          :tags
                                          :quantity
                                          :value
                                          :entity-id
                                          :commodity])))
        expected [{:name "Checking"
                   :type :asset
                   :tags #{}
                   :quantity 0M
                   :value 0M
                   :entity-id entity-id}
                  {:name "Credit card"
                   :type :liability
                   :tags #{}
                   :quantity 0M
                   :value 0M
                   :entity-id entity-id}]]
    (is (= expected actual) "It returns the correct accounts")))

(def ^:private nested-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"
                  :entity-id "Personal"}]
   :entities [{:name "Personal"}]
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
               :parent-id "Taxes"}]})

; TODO Probably this should be moved to mirror the ns where the nest fn is defined
(deftest select-nested-accounts
  (let [context (realize nested-context)
        entity-id (-> context :entities first :id)
        result (->> (accounts/search {:entity-id entity-id})
                    nest
                    simplify-account-groups)
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

(deftest select-account-with-children
  (let [ctx (realize nested-context)
        account (find-account ctx "Savings")
        result (accounts/search {:id (:id account)}
                                {:include-children? true})]
    (is (= #{"Savings" "Reserve" "Car" "Doug" "Eli"}
           (set (map :name result))))))

(deftest create-an-account
  (let [context (realize account-context)
        result (accounts/create (attributes context))
        entity (find-entity context "Personal")
        usd (find-commodity context "USD")
        accounts (->> (accounts/search {:entity-id (:id entity)})
                      (map #(select-keys % [:name
                                            :type
                                            :entity-id
                                            :tags
                                            :commodity-id
                                            :quantity
                                            :value])))
        expected [{:name "Checking"
                   :type :asset
                   :entity-id (:id entity)
                   :tags #{:something-special}
                   :commodity-id (:id usd)
                   :quantity 0M
                   :value 0M}]]
    (is (empty? (v/error-messages result))
        "The result has no validation errors.")
    (pprint-diff expected accounts)
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
   :entities [{:name "Personal"}
              {:name "Business"}]
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
               :entity-id "Personal"}
              {:name "Investment"
               :type :income
               :entity-id "Personal"}]})

(deftest duplicate-name-across-entities
  (let [context (realize duplicate-name-context)
        business (first (filter #(= "Business" (:name %)) (:entities context)))
        result (accounts/create {:name "Credit card"
                                 :type :liability
                                 :entity-id (:id business)})]
    (is (not (v/has-error? result))
        "A second account can be created with the same name in a different entity")))

(deftest duplicate-name-across-parents
  (let [context (realize duplicate-name-context)
        business (first (filter #(= "Business" (:name %)) (:entities context)))
        household (first (filter #(= "Household" (:name %)) (:accounts context)))
        result (accounts/create {:name "Repair"
                                 :type :expense
                                 :parent-id (:id household)
                                 :entity-id (:id business)})]
    (is (empty? (v/error-messages result))
        "A name can be dulicated across parents")))

(deftest duplicate-name-across-asset-types
  (let [context (realize duplicate-name-context)
        entity (find-entity context "Personal")
        result (accounts/create {:name "Investment"
                                 :type :expense
                                 :entity-id (:id entity)})]
    (is (empty? (v/error-messages result))
        "A name can be dulicated across asset types")))

(def ^:private create-child-context
  {:users [(factory :user)]
   :commodities [{:symbol "USD"
                  :type :currency
                  :name "US Dollar"}]
   :entities [{:name "Personal"}]
   :accounts [{:name "Savings"
               :type :asset}]})

(deftest create-a-child-account
  (let [context (realize create-child-context)
        savings (-> context :accounts first)
        entity (-> context :entities first)
        car (accounts/create {:name "Car"
                              :type :asset
                              :parent-id (:id savings)
                              :entity-id (:id entity)})]
    (is (empty? (v/error-messages car))
        "The model should not have any errors")))

(deftest child-must-have-same-type-as-parent
  (let [context (realize create-child-context)
        savings (-> context :accounts first)
        entity (-> context :entities first)
        result (accounts/create {:name "Federal income tax"
                                 :type :expense
                                 :parent-id (:id savings)
                                 :entity-id (:id entity)})]
    (assert-validation-error
      :type
      "Type must match the parent type"
      result)))

(deftest name-is-required
  (let [context (realize account-context)
        attr (-> context
                 attributes
                 (dissoc :name))
        result (accounts/create attr)]
    (assert-validation-error :name "Name is required" result)))

(deftest name-is-unique-within-a-parent
  (let [context (realize duplicate-name-context)
        entity (-> context :entities first)
        auto (first (filter #(= "Auto" (:name %)) (:accounts context)))
        attributes {:name "Repair"
                    :parent-id (:id auto)
                    :entity-id (:id entity)
                    :type :expense}]
    (assert-validation-error
      :name
      "Name is already in use"
      (accounts/create attributes))))

(deftest correct-account-type
  (let [context (realize account-context)
        attr (assoc (attributes context) :type :invalidtype)]
    (assert-validation-error
      :type
      "Type must be one of: expense, equity, liability, income, asset"
      (accounts/create attr))))

(deftest commodity-id-defaults-to-entity-default
  (let [context (realize account-context)
        commodity (-> context :commodities first)
        account (-> context
                    attributes
                    (dissoc :commodity-id))
        result (accounts/create account)]
    (is (= (:id commodity) (:commodity-id result))
        "The specified default commodity is used")))

(deftest update-an-account
  (let [context (realize select-context)
        account (find-account context "Checking")
        result (accounts/update (assoc account :name "New name"))
        retrieved (accounts/find account)]
    (is (not (v/has-error? result))
        (format "Unexpected validation error: %s"
                (v/error-messages result)) )
    (is (= "New name" (:name result)) "The updated account is returned")
    (is (= "New name" (:name retrieved)) "The updated account is retreived")))

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
  (let [context (realize same-parent-context)
        [_ fixed house] (:accounts context)
        updated (assoc house :parent-id (:id fixed))
        result (accounts/update updated)
        retrieved (accounts/reload updated)]
    (is (empty? (v/error-messages result))
        "The result has no validation errors")
    (is (= (:id fixed)
           (:parent-id result))
        "The returned account has the correct parent-id value")
    (is (= (:id fixed)
           (:parent-id retrieved))
        "The retrieved account has the correct parent-id value")) )

(deftest delete-an-account
  (let [context (realize select-context)
        account (-> context :accounts first)
        _ (accounts/delete account)
        accounts (accounts/search {:entity-id (-> context
                                                  :entities
                                                  first
                                                  :id)})]
    (is (not-any? #(= (:id account) (:id %)) accounts)
        "The deleted account is no longer returned from the database")))

(defmacro test-polarization
  [context account-type action quantity expected message]
  `(let [account# (accounts/create (assoc (attributes ~context)
                                          :type ~account-type))
         item# {:account-id (:id account#)
                :action ~action
                :quantity ~quantity}
         polarized# (polarize-quantity item# account#)]
     (is (= ~expected polarized#) ~message)))

(deftest polarize-a-quantity
  (let [context (realize account-context)]
    ; Debits
    (test-polarization context :asset     :debit 100M  100M "A debit in an asset account increases the balance")
    (test-polarization context :expense   :debit 100M  100M "A debit in an expense account increases the balance")
    (test-polarization context :liability :debit 100M -100M "A debit in an liability account decreases the balance")
    (test-polarization context :equity    :debit 100M -100M "A debit in an equity account decreases the balance")
    (test-polarization context :income    :debit 100M -100M "A debit in an income account decreases the balance")

    ;; Credits
    (test-polarization context :asset     :credit 100M -100M "A credit in an asset account decreases the balance")
    (test-polarization context :expense   :credit 100M -100M "A credit in an expense account dereases the balance")
    (test-polarization context :liability :credit 100M  100M "A credit in an liability account increases the balance")
    (test-polarization context :equity    :credit 100M  100M "A credit in an equity account increases the balance")
    (test-polarization context :income    :credit 100M  100M "A credit in an income account increases the balance")))

(deftest derive-action-from-quantity-and-account
  (is (= :debit (derive-action 1 {:type :asset})))
  (is (= :credit (derive-action -1 {:type :asset})))
  (is (= :debit (derive-action 1 {:type :expense})))
  (is (= :credit (derive-action -1 {:type :expense})))
  (is (= :credit (derive-action 1 {:type :income})))
  (is (= :debit (derive-action -1 {:type :income})))
  (is (= :credit (derive-action 1 {:type :equity})))
  (is (= :debit (derive-action -1 {:type :equity})))
  (is (= :credit (derive-action 1 {:type :liability})))
  (is (= :debit (derive-action -1 {:type :liability}))))
