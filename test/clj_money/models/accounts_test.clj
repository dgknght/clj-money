(ns clj-money.models.accounts-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.factories.account-factory]
            [clj-money.test-context :refer [with-context
                                            realize
                                            find-entity
                                            find-commodity
                                            find-account]]
            [clj-money.models.accounts :as accounts]
            [clj-money.accounts :refer [polarize-quantity
                                        derive-action]]
            [clj-money.test-helpers :refer [reset-db]]))

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
   :system-tags #{:something-special}})

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
  (with-context select-context
    (let [entity (find-entity "Personal")
          expected [{:name "Checking"
                     :type :asset
                     :system-tags #{}
                     :quantity 0M
                     :value 0M
                     :entity-id (:id entity)}
                    {:name "Credit card"
                     :type :liability
                     :system-tags #{}
                     :quantity 0M
                     :value 0M
                     :entity-id (:id entity)}]]
      (is (seq-of-maps-like? expected (accounts/search {:entity-id (:id entity)}))
          "It returns the accounts for the entity"))))

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
                                            :system-tags
                                            :commodity-id
                                            :quantity
                                            :value])))
        expected [{:name "Checking"
                   :type :asset
                   :entity-id (:id entity)
                   :system-tags #{:something-special}
                   :commodity-id (:id usd)
                   :quantity 0M
                   :value 0M}]]
    (is (valid? result))
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
    (is (valid? result))))

(deftest duplicate-name-across-parents
  (let [ctx (realize duplicate-name-context)
        business (find-entity ctx "Personal")
        household (find-account ctx "Household")
        result (accounts/create {:name "Repair"
                                 :type :expense
                                 :parent-id (:id household)
                                 :entity-id (:id business)})]
    (is (valid? result))))

(deftest duplicate-name-with-nil-parent
  (let [ctx (realize duplicate-name-context)
        entity (find-entity ctx "Personal")
        result (accounts/create {:name "Repair"
                                 :type :expense
                                 :entity-id (:id entity)})]
    (is (valid? result))))

(deftest duplicate-name-across-asset-types
  (let [context (realize duplicate-name-context)
        entity (find-entity context "Personal")
        result (accounts/create {:name "Investment"
                                 :type :expense
                                 :entity-id (:id entity)})]
    (is (valid? result))))

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
    (is (valid? car))))

(deftest child-must-have-same-type-as-parent
  (let [context (realize create-child-context)
        savings (-> context :accounts first)
        entity (-> context :entities first)
        result (accounts/create {:name "Federal income tax"
                                 :type :expense
                                 :parent-id (:id savings)
                                 :entity-id (:id entity)})]
    (is (invalid? result [:type] "Type must match the parent type"))))

(deftest name-is-required
  (let [context (realize account-context)
        attr (-> context
                 attributes
                 (dissoc :name))
        result (accounts/create attr)]
    (is (invalid? result [:name] "Name is required"))))

(deftest name-is-unique-within-a-parent
  (let [context (realize duplicate-name-context)
        entity (-> context :entities first)
        auto (first (filter #(= "Auto" (:name %)) (:accounts context)))
        attributes {:name "Repair"
                    :parent-id (:id auto)
                    :entity-id (:id entity)
                    :type :expense}]
    (is (invalid? (accounts/create attributes) [:name] "Name is already in use"))))

(deftest correct-account-type
  (let [context (realize account-context)
        attr (assoc (attributes context) :type :invalidtype)]
    (is (invalid? (accounts/create attr)
                  [:type]
                  "Type must be expense, equity, liability, income, or asset"))))

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
    (is (valid? result))
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
    (is (valid? result))
    (is (= (:id fixed)
           (:parent-id result))
        "The returned account has the correct parent-id value")
    (is (= (:id fixed)
           (:parent-id retrieved))
        "The retrieved account has the correct parent-id value")))

(deftest delete-an-account
  (let [context (realize select-context)
        account (-> context :accounts first)
        _ (accounts/delete account)
        retrieved (accounts/find (:id account))]
    (is (nil? retrieved) "The account cannot be retrieved after delete.")))

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
