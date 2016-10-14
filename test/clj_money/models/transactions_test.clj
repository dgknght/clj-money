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
            [clj-money.serialization :as serialization]
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

(defn test-context
  "Returns a context containing related models necessary to run the tests"
  []
  (let [accounts (zipmap [:checking :salary :groceries]
                         (->> account-defs
                              (map #(assoc % :entity-id (:id entity)))
                              (map #(accounts/create storage-spec %))))]
    {:accounts accounts
     :attributes {:transaction-date (t/local-date 2016 3 2)
                  :entity-id (:id entity)
                  :items [{:account-id (-> accounts :checking :id)
                           :action :debit
                           :amount (bigdec 1000)}
                          {:account-id (-> accounts :salary :id)
                           :action :credit
                           :amount (bigdec 1000)}]}}))

(deftest create-a-transaction
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create storage-spec attributes)]
    (testing "return value includes the new id"
      (is (validation/valid? transaction))
      (is (number? (:id transaction)) "A map with the new ID is returned"))
    (testing "transaction can be retrieved"
      (let [retrieved (transactions/find-by-id storage-spec (:id transaction))]
        (is retrieved "The transaction is retrievable by ID")
        (is (= 2
               (count (:items retrieved))) "The items are returned with the transaction")))))

(deftest transaction-date-is-required
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create storage-spec (dissoc attributes :transaction-date))]
    (is (validation/has-error? transaction :transaction-date))))

(deftest entity-id-is-required
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create storage-spec (dissoc attributes :entity-id))]
    (is (validation/has-error? transaction :entity-id))))

(deftest item-account-id-is-required
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create
                      storage-spec
                      (update-in attributes
                                 [:items 0]
                                 #(dissoc % :account-id)))]
    (is (validation/has-error? transaction :items))))

(deftest item-amount-is-required
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create
                      storage-spec
                      (update-in attributes
                                 [:items 0]
                                 #(dissoc % :amount)))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-amount-must-be-greater-than-zero
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create
                      storage-spec
                      (update-in attributes
                                 [:items 0]
                                 #(assoc % :amount (bigdec -1000))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-action-is-required
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create
                      storage-spec
                      (update-in attributes
                                 [:items 0]
                                 #(dissoc % :action)))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-action-must-be-debit-or-created
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create
                      storage-spec
                      (update-in attributes
                                 [:items 0]
                                 #(assoc % :action :not-valid)))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest sum-of-debits-must-equal-sum-of-credits
  (let [context (test-context)
        attributes (:attributes context)
        transaction (transactions/create
                      storage-spec
                      (update-in attributes
                                 [:items 0]
                                 #(assoc % :amount (bigdec 1001))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(def balance-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}]
   :accounts [{:name "Checking"
               :type :asset
               :entity-id "Personal"}
              {:name "Salary"
               :type :income
               :entity-id "Personal"}
              {:name "Groceries"
               :type :expense
               :entity-id "Personal"}]
   :transactions [{:transaction-date (t/local-date 2016 3 2)
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 3)
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 100}
                           {:action :credit
                            :account-id "Checking"
                            :amount 100}]}]})


(deftest item-balances-are-set-when-saved
  (let [context (serialization/realize storage-spec balance-context)
        [checking-items
         salary-items
         groceries-items] (map #(transactions/items-by-account storage-spec (:id %))
                               (:accounts context))]
           ; Transactions are returned with most recent first
    (is (= [(bigdec 900) (bigdec 1000)]
           (map :balance checking-items))
        "The checking account balances are correct")
    (is (= [(bigdec 1000)] (map :balance salary-items))
          "The salary account balances are correct")
    (is (= [(bigdec 100)] (map :balance groceries-items))
          "The groceries account balances are correct")))

(deftest item-indexes-are-set-when-saved
  (let [context (serialization/realize storage-spec balance-context)
        [checking-items
         salary-items
         groceries-items] (map #(transactions/items-by-account storage-spec (:id %))
                               (:accounts context))]
    (is (= [1 0] (map :index checking-items)) "The checking transaction items have correct indexes")
    (is (= [0] (map :index salary-items)) "The salary transaction items have the correct indexes")
    (is (= [0] (map :index groceries-items)) "The groceries transaction items have the correct indexes")))

(deftest account-balances-are-set-when-saved
  (let [context (serialization/realize storage-spec balance-context)
        [checking-balance
         salary-balance
         groceries-balance] (map #(->> %
                                       :id
                                       (accounts/find-by-id storage-spec)
                                       :balance)
                                 (:accounts context))]
    (is (= (bigdec 900) checking-balance) "The checking account has the correct balance.")
    (is (= (bigdec 1000) salary-balance) "The salary account has the correct balance.")
    (is (= (bigdec 100) groceries-balance) "The groceries account has the correct balance.")))

(def insert-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}]
   :accounts [{:name "Checking"
               :type :asset
               :entity-id "Personal"}
              {:name "Salary"
               :type :income
               :entity-id "Personal"}
              {:name "Groceries"
               :type :expense
               :entity-id "Personal"}]
   :transactions [{:transaction-date (t/local-date 2016 3 2)
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 10)
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 100}
                           {:action :credit
                            :account-id "Checking"
                            :amount 100}]}
                  {:transaction-date (t/local-date 2016 3 3)
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 99}
                           {:action :credit
                            :account-id "Checking"
                            :amount 99}]}]})

(deftest insert-transaction-before-the-end
  (let [context (serialization/realize storage-spec insert-context)
        [checking-items
         salary-items
         groceries-items] (map #(transactions/items-by-account storage-spec (:id %))
                               (:accounts context))]
    (is (= [{:index 2
             :amount (bigdec 100)
             :balance (bigdec 801)}
            {:index 1
             :amount (bigdec 99)
             :balance (bigdec 901)}
            {:index 0
             :amount (bigdec 1000)
             :balance (bigdec 1000)}]
           (map #(select-keys % [:index :amount :balance]) checking-items))
        "The checking item balances should be correct")
    (is (= (map bigdec [801 1000 199])
           (map #(:balance (accounts/find-by-id storage-spec (:id %))) (:accounts context)))
        "The checking account has the correct balance")))

(def multi-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}]
   :accounts [{:name "Checking"
               :type :asset
               :entity-id "Personal"}
              {:name "Salary"
               :type :income
               :entity-id "Personal"}
              {:name "Bonus"
               :type :income
               :entity-id "Personal"}
              {:name "Groceries"
               :type :expense
               :entity-id "Personal"}]
   :transactions [{:transaction-date (t/local-date 2016 3 2)
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :debit
                            :account-id "Checking"
                            :amount 100}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}
                           {:action :credit
                            :account-id "Bonus"
                            :amount 100}]}
                  {:transaction-date (t/local-date 2016 3 10)
                   :entity-id "Personal"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 100}
                           {:action :credit
                            :account-id "Checking"
                            :amount 100}]}]})

(deftest create-a-transaction-with-multiple-items-for-one-account
  (let [context (serialization/realize storage-spec multi-context)
        [checking-items
         salary-items
         groceries-items] (map #(transactions/items-by-account storage-spec (:id %))
                               (:accounts context))]
    (is (= [{:index 0 :amount 1000 :balance 1000}
            {:index 1 :amount  100 :balance 1100}
            {:index 2 :amount  100 :balance 1000}]
           checking-items)
        "The checking account items are correct")))

; update a transaction
; change amount
;  subsequent item balances are recalculated
;  account balance is recalculated
; change date
;  subsequent item balances are recalculated
;  subsequent item indexes are recaculated
;  account balances are recalculated
;  recalculation stops once new balance matches old balance
; change account
;  old account balance and items are recalculated
;  new account balance and items are recalculated

; delete a transaction
