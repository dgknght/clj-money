(ns clj-money.models.transactions-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
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
                  :description "Paycheck"
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
               (count (:items retrieved))) "The items are returned with the transaction")
        (is (= (t/local-date 2016 3 2)
               (:transaction-date retrieved))
            "The transaction date is correct")))))

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
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 3)
                   :entity-id "Personal"
                   :description "Kroger"
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
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 10)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 100}
                           {:action :credit
                            :account-id "Checking"
                            :amount 100}]}
                  {:transaction-date (t/local-date 2016 3 3)
                   :entity-id "Personal"
                   :description "Kroger"
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
                   :description "Paycheck"
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
                   :description "Kroger"
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
                               (:accounts context))
        expected-checking-items (->> [{:index 2 :amount  100 :balance 1000}
                                      {:index 1 :amount  100 :balance 1100}
                                      {:index 0 :amount 1000 :balance 1000}]
                                     (map #(update-in % [:amount] bigdec))
                                     (map #(update-in % [:balance] bigdec)))
        actual-checking-items (map #(select-keys % [:index :amount :balance])
                                   checking-items)]
    (is (= expected-checking-items
           actual-checking-items)
        "The checking account items are correct")))

(def delete-context
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
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 3)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 101}
                           {:action :credit
                            :account-id "Checking"
                            :amount 101}]}
                  {:transaction-date (t/local-date 2016 3 4)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 102}
                           {:action :credit
                            :account-id "Checking"
                            :amount 102}]}]})

(deftest delete-a-transaction
  (let [context (serialization/realize storage-spec delete-context)
        [checking
         salary
         groceries] (:accounts context)
        checking-items-before (transactions/items-by-account storage-spec
                                                             (:id checking))
        _ (transactions/delete storage-spec  (-> context
                                                :transactions
                                                second
                                                :id))
        checking-items-after (transactions/items-by-account storage-spec
                                                             (:id checking))]
    (testing "transaction item balances are adjusted"
      (let [expected-before [{:index 2 :amount (bigdec  102) :balance (bigdec  797)}
                             {:index 1 :amount (bigdec  101) :balance (bigdec  899)}
                             {:index 0 :amount (bigdec 1000) :balance (bigdec 1000)}]
            actual-before (map #(select-keys % [:index :amount :balance])
                               checking-items-before)
            expected-after[{:index 1 :amount (bigdec  102) :balance (bigdec  898)}
                           {:index 0 :amount (bigdec 1000) :balance (bigdec 1000)}]
            actual-after (map #(select-keys % [:index :amount :balance]) checking-items-after)]
        (is (= expected-before actual-before)
            "Checking should have the correct items before delete")
        (is (= expected-after actual-after)
            "Checking should have the correct items after delete")))
    (testing "account balances are adjusted"
      (let [checking-after (accounts/find-by-id storage-spec (:id checking))
            groceries-after (accounts/find-by-id storage-spec (:id groceries))]
        (is (= (bigdec 898) (:balance checking-after))
            "Checking should have the correct balance after delete")
        (is (= (bigdec 102) (:balance groceries-after))
            "Groceries should have the correct balance after delete")))))

(def update-context
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
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 12)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 101}
                           {:action :credit
                            :account-id "Checking"
                            :amount 101}]}
                  {:transaction-date (t/local-date 2016 3 22)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 102}
                           {:action :credit
                            :account-id "Checking"
                            :amount 102}]}]})

(deftest update-a-transaction-change-amount
  (let [context (serialization/realize storage-spec update-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3] (:transactions context)
        updated (-> t2
                    (assoc-in [:items 0 :amount] (bigdec 99.99))
                    (assoc-in [:items 1 :amount] (bigdec 99.99)))
        _ (transactions/update storage-spec updated)
        expected-checking [{:index 2 :amount (bigdec  102)    :balance (bigdec  798.01)}
                           {:index 1 :amount (bigdec   99.99) :balance (bigdec  900.01)}
                           {:index 0 :amount (bigdec 1000)    :balance (bigdec 1000)}]
        actual-checking (->> (:id checking)
                             (transactions/items-by-account storage-spec)
                             (map #(select-keys % [:index :amount :balance]))) 
        expected-groceries [{:index 1 :amount (bigdec 102)    :balance (bigdec 201.99)}
                            {:index 0 :amount (bigdec  99.99) :balance (bigdec  99.99)}]
        actual-groceries (->> (:id groceries)
                              (transactions/items-by-account storage-spec)
                              (map #(select-keys % [:index :amount :balance])))]
    (testing "transaction item balances are correct"
      (is (= expected-checking actual-checking)
          "Check items should have the correct values after update")
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (testing "account balances are correct"
      (is (= (bigdec 798.01) (->> checking
                                  (accounts/reload storage-spec)
                                  :balance))
          "The checkout account balance should be correct after update")
      (is (= (bigdec 201.99) (->> groceries
                                  (accounts/reload storage-spec)
                                  :balance))
          "The groceries account balance should be correct after update"))))

(deftest update-a-transaction-change-date
  (let [context (serialization/realize storage-spec update-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3] (:transactions context)
        updated (assoc t3 :transaction-date (t/local-date 2016 3 10))
        _ (transactions/update storage-spec updated)
        expected-checking [{:index 2 :amount (bigdec  101) :balance (bigdec  797)}
                           {:index 1 :amount (bigdec  102) :balance (bigdec  898)}
                           {:index 0 :amount (bigdec 1000) :balance (bigdec 1000)}]
        actual-checking (->> (:id checking)
                             (transactions/items-by-account storage-spec)
                             (map #(select-keys % [:index :amount :balance]))) 
        expected-groceries [{:index 1 :amount (bigdec 101) :balance (bigdec 203)}
                            {:index 0 :amount (bigdec 102) :balance (bigdec 102)}]
        actual-groceries (->> (:id groceries)
                              (transactions/items-by-account storage-spec)
                              (map #(select-keys % [:index :amount :balance])))]
    (testing "transaction item balances are correct"
      (is (= expected-checking actual-checking)
          "Check items should have the correct values after update")
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (testing "account balances are correct"
      (is (= (bigdec 797) (->> checking
                               (accounts/reload storage-spec)
                               :balance))
          "The checkout account balance should be correct after update")
      (is (= (bigdec 203) (->> groceries
                               (accounts/reload storage-spec)
                               :balance))
          "The groceries account balance should be correct after update"))
    (testing "transaction is updated"
      (is (= (t/local-date 2016 3 10)
             (:transaction-date (transactions/reload storage-spec t3)))
          "The transaction should be updated"))))

(def short-circuit-context
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
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 9)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 101}
                           {:action :credit
                            :account-id "Checking"
                            :amount 101}]}
                  {:transaction-date (t/local-date 2016 3 16)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 102}
                           {:action :credit
                            :account-id "Checking"
                            :amount 102}]}
                  {:transaction-date (t/local-date 2016 3 23)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 103}
                           {:action :credit
                            :account-id "Checking"
                            :amount 103}]}
                  {:transaction-date (t/local-date 2016 3 30)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 104}
                           {:action :credit
                            :account-id "Checking"
                            :amount 104}]}]})

(defn- record-update-call
  [item result]
  (update-in result
             [(:account-id item)]
             #((fnil conj #{}) % (select-keys item [:index
                                                   :amount
                                                   :balance]))))

(defn- fake-update-item-index-and-balance
  [context update-calls storage-spec item]
  (swap! update-calls (partial record-update-call item))
  (let [existing-item (->> context
                           :transactions
                           (mapcat :items)
                           (filter #(= (:id item) (:id %)))
                           first)]
    (not= (select-keys item [:index :balance])
          (select-keys existing-item
                       [:index :balance]))))

(deftest update-a-transaction-short-circuit-updates
  (let [context (serialization/realize storage-spec short-circuit-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3 t4] (:transactions context)
        updated (assoc t3 :transaction-date (t/local-date 2016 3 8))
        update-calls (atom {})]
    (with-redefs [transactions/update-item-index-and-balance (partial fake-update-item-index-and-balance context update-calls)]
      (transactions/update storage-spec updated)
      (let [expected #{{:index 2
                       :amount (bigdec 101)
                       :balance (bigdec 797)}
                      {:index 3
                       :amount (bigdec 103)
                       :balance (bigdec 694)}} ; The first update that doesn't change a value stops the chain
                                               ; the 4th item should never be updated because the 4rd one did not change a value
            actual (get @update-calls (:id checking))]
        (testing "the expected transactions are updated"
          (is (= expected actual)
              "Only items with changes are updated")
          (is (not-any? #(= (:index %) 4) actual) "The last item is never updated"))
        (testing "account balances are correct"
          (is (= (bigdec 590) (:balance (accounts/reload storage-spec checking))) "Checking has the correct balance"))))))

(def change-account-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}
              {:name "Rent"
               :type :expense}
              {:name "Groceries"
               :type :expense}]
   :transactions [{:transaction-date (t/local-date 2016 3 2)
                   :entity-id "Personal"
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 9)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 101}
                           {:action :credit
                            :account-id "Checking"
                            :amount 101}]}
                  {:transaction-date (t/local-date 2016 3 16)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 102}
                           {:action :credit
                            :account-id "Checking"
                            :amount 102}]}
                  {:transaction-date (t/local-date 2016 3 23)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 103}
                           {:action :credit
                            :account-id "Checking"
                            :amount 103}]}]})

(deftest update-a-transaction-change-account
  (let [context (serialization/realize storage-spec change-account-context)
        [checking
         salary
         rent
         groceries] (:accounts context)
        [t1 t2 t3 t4] (:transactions context)
        _ (transactions/update storage-spec (assoc-in t3 [:items 0 :account-id] (:id rent)))
        groceries-items (transactions/items-by-account storage-spec (:id groceries))
        rent-items (transactions/items-by-account storage-spec (:id rent))
        expected-groceries [{:index 1
                             :amount (bigdec 103)
                             :balance (bigdec 204)}
                            {:index 0
                             :amount (bigdec 101)
                             :balance (bigdec 101)}]
        expected-rent [{:index 0
                        :amount (bigdec 102)
                        :balance (bigdec 102)}]
        actual-groceries (map #(select-keys % [:index :amount :balance])
                              groceries-items)]
    (testing "accounts have the correct items"
      (is (= expected-groceries actual-groceries)
          "Groceries should have the correct items after update")
      (is (= expected-rent
             (map #(select-keys % [:index :amount :balance]) rent-items))))
    (testing "accounts have the correct balances"
      (is (= (bigdec 204)
             (:balance (accounts/reload storage-spec groceries)))
          "The groceries account has the correct balance after update")
      (is (= (bigdec 102)
             (:balance (accounts/reload storage-spec rent)))
          "The rend account has the correct balance after update"))))

(def change-action-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}
              {:name "Groceries"
               :type :expense}]
   :transactions [{:transaction-date (t/local-date 2016 3 2)
                   :entity-id "Personal"
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 9)
                   :entity-id "Personal"
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 103}
                           {:action :credit
                            :account-id "Checking"
                            :amount 103}]}
                  {:transaction-date (t/local-date 2016 3 16)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 12}
                           {:action :credit
                            :account-id "Checking"
                            :amount 12}]}
                  {:transaction-date (t/local-date 2016 3 23)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 101}
                           {:action :credit
                            :account-id "Checking"
                            :amount 101}]}]})

(deftest update-a-transaction-change-action
  (let [context (serialization/realize storage-spec change-action-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3 t4] (:transactions context)
        updated (transactions/update storage-spec (-> t3
                                                      (assoc-in [:items 0 :action] :credit)
                                                      (assoc-in [:items 1 :action] :debit)))
        expected-items [{:index 2
                         :amount (bigdec 101)
                         :balance (bigdec 192)}
                        {:index 1
                         :amount (bigdec 12)
                         :balance (bigdec 91)}
                        {:index 0
                         :amount (bigdec 103)
                         :balance (bigdec 103)}]
        actual-items (map #(select-keys % [:index :amount :balance])
                          (transactions/items-by-account storage-spec (:id groceries)))]
    (testing "items are updated correctly"
      (is (= expected-items actual-items)
          "Groceries should have the correct items after update"))
    (testing "account balances are updated correctly"
      (is (= (bigdec 192)
             (:balance (accounts/reload storage-spec groceries)))
          "Groceries should have the correct balance after update")
      (is (= (bigdec 808)
             (:balance (accounts/reload storage-spec checking)))
          "Checking should have the correct balance after update"))))

(def add-remove-item-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}
              {:name "Pets"
               :type :expense}
              {:name "Groceries"
               :type :expense}]
   :transactions [{:transaction-date (t/local-date 2016 3 2)
                   :entity-id "Personal"
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000}]}
                  {:transaction-date (t/local-date 2016 3 9)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 103}
                           {:action :credit
                            :account-id "Checking"
                            :amount 103}]}
                  {:transaction-date (t/local-date 2016 3 16)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 90}
                           {:action :debit
                            :account-id "Pets"
                            :amount 12}
                           {:action :credit
                            :account-id "Checking"
                            :amount 102}]}
                  {:transaction-date (t/local-date 2016 3 23)
                   :entity-id "Personal"
                   :description "Kroger"
                   :items [{:action :debit
                            :account-id "Groceries"
                            :amount 101}
                           {:action :credit
                            :account-id "Checking"
                            :amount 101}]}]})

(deftest update-a-transaction-remove-item
  (let [context (serialization/realize storage-spec add-remove-item-context)
        [checking
         salary
         pets
         groceries] (:accounts context)
        [t1 t2 t3 t4] (:transactions context)
        to-update (-> t3
                      (assoc-in [:items 0 :amount] (bigdec 102))
                      (update-in [:items] #(remove (fn [item]
                                                     (= (:account-id item)
                                                        (:id pets)))
                                                   %)))
        updated (transactions/update storage-spec to-update)
        expected-items [{:index 2
                         :amount (bigdec 101)
                         :balance (bigdec 306)}
                        {:index 1
                         :amount (bigdec 102)
                         :balance (bigdec 205)}
                        {:index 0
                         :amount (bigdec 103)
                         :balance (bigdec 103)}]
        actual-items (map #(select-keys % [:index :amount :balance])
                          (transactions/items-by-account storage-spec (:id groceries)))]
    (testing "item values are correct"
      (is (= expected-items actual-items)
          "The Pets account should have the correct items"))
    (testing "account balances are correct"
      (is (= (bigdec 0)
             (:balance (accounts/reload storage-spec pets)))
          "Pets should have the correct balance after update")
      (is (= (bigdec 306)
             (:balance (accounts/reload storage-spec groceries)))
          "Groceries should have the correct balance after update")
      (is (= (bigdec 694)
             (:balance (accounts/reload storage-spec checking)))
          "Checking should have the correct balance after update"))))

(deftest update-a-transaction-add-item
  (let [context (serialization/realize storage-spec add-remove-item-context)
        [checking
         salary
         pets
         groceries] (:accounts context)
        [t1 t2 t3 t4] (:transactions context)
        to-update (-> t2
                      (assoc-in [:items 0 :amount] (bigdec 90))
                      (update-in [:items] #(conj % {:action :debit
                                                    :account-id (:id pets)
                                                    :amount (bigdec 13)})))
        updated (transactions/update storage-spec to-update)
        expected-items [{:index 1
                         :amount (bigdec 12)
                         :balance (bigdec 25)}
                        {:index 0
                         :amount (bigdec 13)
                         :balance (bigdec 13)}]
        actual-items (map #(select-keys % [:index :amount :balance])
                          (transactions/items-by-account storage-spec (:id pets)))]
    (testing "item values are correct"
      (is (= expected-items actual-items)
          "The Pets account should have the correct items"))
    (testing "account balances are correct"
      (is (= (bigdec 25)
             (:balance (accounts/reload storage-spec pets)))
          "Pets should have the correct balance after update")
      (is (= (bigdec 281)
             (:balance (accounts/reload storage-spec groceries)))
          "Groceries should have the correct balance after update")
      (is (= (bigdec 694)
             (:balance (accounts/reload storage-spec checking)))
          "Checking should have the correct balance after update"))))
