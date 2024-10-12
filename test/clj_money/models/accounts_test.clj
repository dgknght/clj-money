(ns clj-money.models.accounts-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.factories.account-factory]
            [clj-money.db :as db]
            [clj-money.db.sql.ref]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-commodity
                                            find-account]]
            [clj-money.model-helpers :as helpers :refer [assert-invalid]]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]
            [clj-money.accounts :refer [polarize-quantity
                                        derive-action]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(defn- attributes []
  #:account{:name "Checking"
            :type :asset
            :entity (find-entity "Personal")
            :system-tags #{:something-special}})

(def ^:private account-context
  {:users [(factory :user #:user{:email "john@doe.com"})]
   :commodities [#:commodity{:symbol "USD"
                             :type :currency
                             :name "US Dollar"
                             :entity "Personal"}]
   :entities [#:entity{:name "Personal"
                       :user "john@doe.com"}]})

(def ^:private select-context
  (assoc account-context
         :accounts [#:account{:name "Credit card"
                              :entity "Personal"
                              :type :liability}
                    #:account{:name "Checking"
                              :entity "Personal"
                              :type :asset}]))

(deftest select-accounts
  (with-context select-context
    (let [entity (find-entity "Personal")
          expected [#:account{:name "Checking"
                              :type :asset
                              :system-tags #{}
                              :quantity 0M
                              :value 0M
                              :entity (select-keys entity [:id])}
                    #:account{:name "Credit card"
                              :type :liability
                              :system-tags #{}
                              :quantity 0M
                              :value 0M
                              :entity (select-keys entity [:id])}]]
      (is (seq-of-maps-like? expected (models/select #:account{:entity entity}))
          "It returns the accounts for the entity"))))

(def ^:private nested-context
  (-> select-context
      (update-in [:accounts] concat [#:account{:name "Savings"
                                               :type :asset
                                               :entity "Personal"}
                                     #:account{:name "Reserve"
                                               :type :asset
                                               :parent "Savings"
                                               :entity "Personal"}
                                     #:account{:name "Car"
                                               :type :asset
                                               :parent "Savings"
                                               :entity "Personal"}
                                     #:account{:name "Doug"
                                               :type :asset
                                               :parent "Car"
                                               :entity "Personal"}
                                     #:account{:name "Eli"
                                               :type :asset
                                               :parent "Car"
                                               :entity "Personal"}
                                     #:account{:name "Checking"
                                               :type :asset
                                               :entity "Personal"}
                                     #:account{:name "Taxes"
                                               :type :expense
                                               :entity "Personal"}
                                     #:account{:name "Federal Income Tax"
                                               :type :expense
                                               :parent "Taxes"
                                               :entity "Personal"}
                                     #:account{:name "Social Security"
                                               :type :expense
                                               :parent "Taxes"
                                               :entity "Personal"}])))

(deftest select-account-with-children
  (with-context nested-context
    (let [account (find-account "Savings")
          result (models/select ^{::db/model-type :account} {:id (:id account)}
                                {:include-children? true})]
      (is (= #{"Savings" "Reserve" "Car" "Doug" "Eli"}
             (set (map :account/name result)))))))

; (defn- assert-created
;   [attr]
;   (helpers/assert-created attr :refs [:account/entity :account/commodity]))
; 
; (deftest create-an-account
;   (with-context account-context
;     (assert-created (attributes))))
; 
; (def ^:private duplicate-name-context
;   (-> select-context
;       (update-in [:entities] conj #:entity{:name "Business"})
;       (update-in [:commodities] conj #:commodity{:symbol "USD"
;                                                  :name "US Dollar"
;                                                  :entity "Business"})
;       (update-in [:accounts] concat [#:account{:name "Savings"
;                                                :type :asset
;                                                :entity "Personal"}
;                                      #:account{:name "Household"
;                                                :type :expense
;                                                :entity "Personal"}
;                                      #:account{:name "Repairs"
;                                                :type :expense
;                                                :entity "Personal"
;                                                :parent "Household"}])))
; 
; (deftest name-can-be-duplicated-across-entities
;   (with-context duplicate-name-context
;     (assert-created (attributes))))
; 
; (deftest name-can-be-duplicated-across-parents
;   (with-context duplicate-name-context
;     (assert-created (assoc (attributes)
;                            :account/parent (find-account ["Personal" "Savings"])
;                            :account/name "Repairs"))))
; 
; (deftest name-can-be-duplicated-across-account-types
;   (with-context duplicate-name-context
;     (assert-created (assoc (attributes)
;                            :account/type :liabilities
;                            :account/name "Household"))))
; 
; (def ^:private create-child-context
;   {:users [(factory :user)]
;    :commodities [{:symbol "USD"
;                   :type :currency
;                   :name "US Dollar"}]
;    :entities [{:name "Personal"}]
;    :accounts [{:name "Savings"
;                :type :asset}]})
; 
; (deftest create-a-child-account
;   (let [context (realize create-child-context)
;         savings (-> context :accounts first)
;         entity (-> context :entities first)
;         car (accounts/create {:name "Car"
;                               :type :asset
;                               :parent-id (:id savings)
;                               :entity-id (:id entity)})]
;     (is (valid? car))))
; 
; (deftest child-must-have-same-type-as-parent
;   (let [context (realize create-child-context)
;         savings (-> context :accounts first)
;         entity (-> context :entities first)
;         result (accounts/create {:name "Federal income tax"
;                                  :type :expense
;                                  :parent-id (:id savings)
;                                  :entity-id (:id entity)})]
;     (is (invalid? result [:type] "Type must match the parent type"))))
; 
; (deftest name-is-required
;   (let [context (realize account-context)
;         attr (-> context
;                  attributes
;                  (dissoc :name))
;         result (accounts/create attr)]
;     (is (invalid? result [:name] "Name is required"))))
; 
; (deftest name-is-unique-within-a-parent
;   (let [context (realize duplicate-name-context)
;         entity (-> context :entities first)
;         auto (first (filter #(= "Auto" (:name %)) (:accounts context)))
;         attributes {:name "Repair"
;                     :parent-id (:id auto)
;                     :entity-id (:id entity)
;                     :type :expense}]
;     (is (invalid? (accounts/create attributes) [:name] "Name is already in use"))))
; 
; (deftest correct-account-type
;   (let [context (realize account-context)
;         attr (assoc (attributes context) :type :invalidtype)]
;     (is (invalid? (accounts/create attr)
;                   [:type]
;                   "Type must be expense, equity, liability, income, or asset"))))
; 
; (deftest commodity-id-defaults-to-entity-default
;   (let [context (realize account-context)
;         commodity (-> context :commodities first)
;         account (-> context
;                     attributes
;                     (dissoc :commodity-id))
;         result (accounts/create account)]
;     (is (= (:id commodity) (:commodity-id result))
;         "The specified default commodity is used")))
; 
; (deftest update-an-account
;   (let [context (realize select-context)
;         account (find-account context "Checking")
;         result (accounts/update (assoc account
;                                        :name "New name"
;                                        :allocations {1 50M ; we wouldn't really set them on checking, but rather IRA, or 401k
;                                                      2 50M}))
;         retrieved (accounts/find account)]
;     (is (valid? result))
;     (is (comparable? {:name "New name"
;                       :allocations {1 50M
;                                     2 50M}}
;                      result)
;         "The updated account is returned")
;     (is (comparable? {:name "New name"
;                       :allocations {1 50M
;                                     2 50M}}
;                      retrieved)
;         "The updated account is retreived")))
; 
; (def same-parent-context
;   {:users [(factory :user)]
;    :entities [{:name "Personal"}]
;    :commodities [{:name "US Dollar"
;                   :symbol "USD"
;                   :type :currency}]
;    :accounts [{:name "Current assets"
;                :type :asset}
;               {:name "Fixed assets"
;                :type :asset}
;               {:name "House"
;                :type :asset
;                :parent-id "Current assets"}]})
; 
; (deftest change-an-account-parent
;   (let [context (realize same-parent-context)
;         [_ fixed house] (:accounts context)
;         updated (assoc house :parent-id (:id fixed))
;         result (accounts/update updated)
;         retrieved (accounts/reload updated)]
;     (is (valid? result))
;     (is (= (:id fixed)
;            (:parent-id result))
;         "The returned account has the correct parent-id value")
;     (is (= (:id fixed)
;            (:parent-id retrieved))
;         "The retrieved account has the correct parent-id value")))
; 
; (deftest delete-an-account
;   (let [context (realize select-context)
;         account (-> context :accounts first)
;         _ (accounts/delete account)
;         retrieved (accounts/find (:id account))]
;     (is (nil? retrieved) "The account cannot be retrieved after delete.")))
; 
; (defmacro test-polarization
;   [context account-type action quantity expected message]
;   `(let [account# (accounts/create (assoc (attributes ~context)
;                                           :type ~account-type))
;          item# {:account-id (:id account#)
;                 :action ~action
;                 :quantity ~quantity}
;          polarized# (polarize-quantity item# account#)]
;      (is (= ~expected polarized#) ~message)))
; 
; (deftest polarize-a-quantity
;   (let [context (realize account-context)]
;     ; Debits
;     (test-polarization context :asset     :debit 100M  100M "A debit in an asset account increases the balance")
;     (test-polarization context :expense   :debit 100M  100M "A debit in an expense account increases the balance")
;     (test-polarization context :liability :debit 100M -100M "A debit in an liability account decreases the balance")
;     (test-polarization context :equity    :debit 100M -100M "A debit in an equity account decreases the balance")
;     (test-polarization context :income    :debit 100M -100M "A debit in an income account decreases the balance")
; 
;     ;; Credits
;     (test-polarization context :asset     :credit 100M -100M "A credit in an asset account decreases the balance")
;     (test-polarization context :expense   :credit 100M -100M "A credit in an expense account dereases the balance")
;     (test-polarization context :liability :credit 100M  100M "A credit in an liability account increases the balance")
;     (test-polarization context :equity    :credit 100M  100M "A credit in an equity account increases the balance")
;     (test-polarization context :income    :credit 100M  100M "A credit in an income account increases the balance")))
; 
; (deftest derive-action-from-quantity-and-account
;   (is (= :debit (derive-action 1 {:type :asset})))
;   (is (= :credit (derive-action -1 {:type :asset})))
;   (is (= :debit (derive-action 1 {:type :expense})))
;   (is (= :credit (derive-action -1 {:type :expense})))
;   (is (= :credit (derive-action 1 {:type :income})))
;   (is (= :debit (derive-action -1 {:type :income})))
;   (is (= :credit (derive-action 1 {:type :equity})))
;   (is (= :debit (derive-action -1 {:type :equity})))
;   (is (= :credit (derive-action 1 {:type :liability})))
;   (is (= :debit (derive-action -1 {:type :liability}))))
