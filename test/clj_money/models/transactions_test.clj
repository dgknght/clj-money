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

(def create-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset }
              {:name "Salary"
               :type :income}
              {:name "Groceries"
               :type :expense}]})

(defn attributes
  [context]
  (let [[checking
         salary
         groceries] (:accounts context)]
    {:transaction-date (t/local-date 2016 3 2)
     :description "Paycheck"
     :memo "final, partial"
     :entity-id (-> context :entities first :id)
     :items [{:account-id (:id checking)
              :action :debit
              :memo "conf # 123"
              :amount 1000M}
             {:account-id (:id salary)
              :action :credit
              :amount 1000M}]}))

(deftest create-a-transaction
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create storage-spec (attributes context))]
    (testing "return value includes the new id"
      (is (empty? (validation/error-messages transaction)))
      (is (number? (:id transaction)) "A map with the new ID is returned"))
    (testing "transaction can be retrieved"
      (let [retrieved (transactions/find-by-id storage-spec (:id transaction))]
        (is retrieved "The transaction is retrievable by ID")
        (is (= 2
               (count (:items retrieved))) "The items are returned with the transaction")
        (is (= (t/local-date 2016 3 2)
               (:transaction-date retrieved))
            "The transaction date is correct")
        (is (= "final, partial" (:memo retrieved)) "The transaction memo is correct")
        (is (= ["conf # 123" nil] (map :memo (:items retrieved))) "The item memos are correct")))))

(deftest rollback-on-failure
  (let [call-count (atom 0)]
    (with-redefs [transactions/before-save-item (fn [item]
                                                  (if (= 1 @call-count)
                                                    (throw (RuntimeException. "Induced error"))
                                                    (do
                                                      (swap! call-count inc)
                                                      (update-in item [:action] name))))]
      (let [context (serialization/realize storage-spec create-context)
            [checking
             salary
             groceries] (:accounts context)
            transaction (try
                          (transactions/create storage-spec (attributes context))
                          (catch RuntimeException e
                            nil))]
        (testing "records are not created"
          (is (= 0 (count (transactions/select-by-entity-id storage-spec (-> context :entities first :id))))
              "The transaction should not be saved")
          (is (= 0 (count (transactions/items-by-account storage-spec (:id checking))))
              "The transaction item for checking should not be created")
          (is (= 0 (count (transactions/items-by-account storage-spec (:id salary))))
              "The transaction item for salary should not be created"))
        (testing "account balances are not updated"
          (is (= 0M (->> checking
                                 (accounts/reload storage-spec)
                                 :balance))
              "The checking balance should not be updated")
          (is (= 0M (->> salary
                                 (accounts/reload storage-spec)
                                 :balance))
              "The salary balance should not be updated"))))))

(deftest create-a-transaction-us-string-date
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (assoc :transaction-date "3/2/2016")))]
    (is (empty? (validation/error-messages transaction)) "The transaction is valid")
    (is (= (t/local-date 2016 3 2) (:transaction-date transaction)) "The transaction date is parsed correctly")))

(deftest create-a-transaction-intl-string-date
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (assoc :transaction-date "2016-03-02")))]
    (is (empty? (validation/error-messages transaction)) "The transaction is valid")
    (is (= (t/local-date 2016 3 2) (:transaction-date transaction)) "The transaction date is parsed correctly")))

(deftest transaction-date-is-required
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (dissoc :transaction-date)))]
    (is (validation/has-error? transaction :transaction-date))))

(deftest entity-id-is-required
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (dissoc :entity-id)))]
    (is (validation/has-error? transaction :entity-id))))

(deftest items-are-required
  (let [context (serialization/realize storage-spec create-context)]
    (assert-validation-error :items "Count must be greater than or equal to 2"
                             (transactions/create
                               storage-spec
                               (-> context
                                   attributes
                                   (assoc :items []))))))

(deftest item-account-id-is-required
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(dissoc % :account-id))))]
    (is (validation/has-error? transaction :items))))

(deftest item-amount-is-required
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(dissoc % :amount))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-amount-must-be-greater-than-zero
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(assoc % :amount -1000M))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-action-is-required
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(dissoc % :action))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-action-must-be-debit-or-created
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(assoc % :action :not-valid))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest sum-of-debits-must-equal-sum-of-credits
  (let [context (serialization/realize storage-spec create-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(assoc % :amount 1001M))))]
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
    (is (= [900M 1000M]
           (map :balance checking-items))
        "The checking account balances are correct")
    (is (= [1000M] (map :balance salary-items))
          "The salary account balances are correct")
    (is (= [100M] (map :balance groceries-items))
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
    (is (= 900M checking-balance) "The checking account has the correct balance.")
    (is (= 1000M salary-balance) "The salary account has the correct balance.")
    (is (= 100M groceries-balance) "The groceries account has the correct balance.")))

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
             :amount 100M
             :balance 801M}
            {:index 1
             :amount 99M
             :balance 901M}
            {:index 0
             :amount 1000M
             :balance 1000M}]
           (map #(select-keys % [:index :amount :balance]) checking-items))
        "The checking item balances should be correct")
    (is (= [801M 1000M 199M]
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
        expected-checking-items [{:index 2 :amount  100M :balance 1000M}
                                      {:index 1 :amount  100M :balance 1100M}
                                      {:index 0 :amount 1000M :balance 1000M}]
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
      (let [expected-before [{:index 2 :amount 102M :balance 797M}
                             {:index 1 :amount 101M :balance 899M}
                             {:index 0 :amount 1000M :balance 1000M}]
            actual-before (map #(select-keys % [:index :amount :balance])
                               checking-items-before)
            expected-after[{:index 1 :amount 102M :balance 898M}
                           {:index 0 :amount 1000M :balance 1000M}]
            actual-after (map #(select-keys % [:index :amount :balance]) checking-items-after)]
        (is (= expected-before actual-before)
            "Checking should have the correct items before delete")
        (is (= expected-after actual-after)
            "Checking should have the correct items after delete")))
    (testing "account balances are adjusted"
      (let [checking-after (accounts/find-by-id storage-spec (:id checking))
            groceries-after (accounts/find-by-id storage-spec (:id groceries))]
        (is (= 898M (:balance checking-after))
            "Checking should have the correct balance after delete")
        (is (= 102M (:balance groceries-after))
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
                    (assoc-in [:items 0 :amount] 99.99M)
                    (assoc-in [:items 1 :amount] 99.99M))
        _ (transactions/update storage-spec updated)
        expected-checking [{:index 2 :amount 102M   :balance 798.01M}
                           {:index 1 :amount 99.99M :balance 900.01M}
                           {:index 0 :amount 1000M  :balance 1000M}]
        actual-checking (->> (:id checking)
                             (transactions/items-by-account storage-spec)
                             (map #(select-keys % [:index :amount :balance])))
        expected-groceries [{:index 1 :amount 102M   :balance 201.99M}
                            {:index 0 :amount 99.99M :balance 99.99M}]
        actual-groceries (->> (:id groceries)
                              (transactions/items-by-account storage-spec)
                              (map #(select-keys % [:index :amount :balance])))]
    (testing "transaction item balances are correct"
      (is (= expected-checking actual-checking)
          "Checking items should have the correct values after update")
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (testing "account balances are correct"
      (is (= 798.01M (->> checking
                                  (accounts/reload storage-spec)
                                  :balance))
          "The checkout account balance should be correct after update")
      (is (= 201.99M (->> groceries
                                  (accounts/reload storage-spec)
                                  :balance))
          "The groceries account balance should be correct after update"))))

(deftest rollback-a-failed-update
  (let [real-reload transactions/reload
        call-count (atom 0)]
    (with-redefs [transactions/reload (fn [storage-spec transaction]
                                        (swap! call-count inc)
                                        (if (= 2 @call-count)
                                          (throw (RuntimeException. "Induced exception"))
                                          (real-reload storage-spec transaction)))]
      (let [context (serialization/realize storage-spec update-context)
            [checking
             salary
             groceries] (:accounts context)
            [t1 t2 t3] (:transactions context)
            updated (-> t2
                        (assoc-in [:items 0 :amount] 99.99M)
                        (assoc-in [:items 1 :amount] 99.99M))
            _ (try
                (transactions/update storage-spec updated)
                (catch RuntimeException e nil))]
        (testing "transaction items are not updated"
          (is (= #{101M} (->> t2
                            (transactions/reload storage-spec)
                            :items
                            (map :amount)
                            (into #{})))))
        (testing "account balances are not updated"
          (is (= 797M (->> checking
                                   (accounts/reload storage-spec)
                                   :balance))
              "The checkout account balance should not be changed")
          (is (= 203M (->> groceries
                                   (accounts/reload storage-spec)
                                   :balance))
              "The groceries account balance should not be changed"))))))

(deftest update-a-transaction-change-date
  (let [context (serialization/realize storage-spec update-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3] (:transactions context)
        updated (assoc t3 :transaction-date (t/local-date 2016 3 10))
        _ (transactions/update storage-spec updated)
        expected-checking [{:index 2 :amount 101M :balance 797M}
                           {:index 1 :amount 102M :balance 898M}
                           {:index 0 :amount 1000M :balance 1000M}]
        actual-checking (->> (:id checking)
                             (transactions/items-by-account storage-spec)
                             (map #(select-keys % [:index :amount :balance])))
        expected-groceries [{:index 1 :amount 101M :balance 203M}
                            {:index 0 :amount 102M :balance 102M}]
        actual-groceries (->> (:id groceries)
                              (transactions/items-by-account storage-spec)
                              (map #(select-keys % [:index :amount :balance])))]
    (testing "transaction item balances are correct"
      (is (= expected-checking actual-checking)
          "Check items should have the correct values after update")
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (testing "account balances are correct"
      (is (= 797M (->> checking
                               (accounts/reload storage-spec)
                               :balance))
          "The checkout account balance should be correct after update")
      (is (= 203M (->> groceries
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
                       :amount 101M
                       :balance 797M}
                      {:index 3
                       :amount 103M
                       :balance 694M}} ; The first update that doesn't change a value stops the chain
                                               ; the 4th item should never be updated because the 4rd one did not change a value
            actual (get @update-calls (:id checking))]
        (testing "the expected transactions are updated"
          (is (= expected actual)
              "Only items with changes are updated")
          (is (not-any? #(= (:index %) 4) actual) "The last item is never updated"))
        (testing "account balances are correct"
          (is (= 590M (:balance (accounts/reload storage-spec checking))) "Checking has the correct balance"))))))

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
                             :amount 103M
                             :balance 204M}
                            {:index 0
                             :amount 101M
                             :balance 101M}]
        expected-rent [{:index 0
                        :amount 102M
                        :balance 102M}]
        actual-groceries (map #(select-keys % [:index :amount :balance])
                              groceries-items)]
    (testing "accounts have the correct items"
      (is (= expected-groceries actual-groceries)
          "Groceries should have the correct items after update")
      (is (= expected-rent
             (map #(select-keys % [:index :amount :balance]) rent-items))))
    (testing "accounts have the correct balances"
      (is (= 204M
             (:balance (accounts/reload storage-spec groceries)))
          "The groceries account has the correct balance after update")
      (is (= 102M
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
                         :amount 101M
                         :balance 192M}
                        {:index 1
                         :amount 12M
                         :balance 91M}
                        {:index 0
                         :amount 103M
                         :balance 103M}]
        actual-items (map #(select-keys % [:index :amount :balance])
                          (transactions/items-by-account storage-spec (:id groceries)))]
    (testing "items are updated correctly"
      (is (= expected-items actual-items)
          "Groceries should have the correct items after update"))
    (testing "account balances are updated correctly"
      (is (= 192M
             (:balance (accounts/reload storage-spec groceries)))
          "Groceries should have the correct balance after update")
      (is (= 808M
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
                      (assoc-in [:items 0 :amount] 102M)
                      (update-in [:items] #(remove (fn [item]
                                                     (= (:account-id item)
                                                        (:id pets)))
                                                   %)))
        updated (transactions/update storage-spec to-update)
        expected-items [{:index 2
                         :amount 101M
                         :balance 306M}
                        {:index 1
                         :amount 102M
                         :balance 205M}
                        {:index 0
                         :amount 103M
                         :balance 103M}]
        actual-items (map #(select-keys % [:index :amount :balance])
                          (transactions/items-by-account storage-spec (:id groceries)))]
    (testing "item values are correct"
      (is (= expected-items actual-items)
          "The Pets account should have the correct items"))
    (testing "account balances are correct"
      (is (= 0M
             (:balance (accounts/reload storage-spec pets)))
          "Pets should have the correct balance after update")
      (is (= 306M
             (:balance (accounts/reload storage-spec groceries)))
          "Groceries should have the correct balance after update")
      (is (= 694M
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
                      (assoc-in [:items 0 :amount] 90M)
                      (update-in [:items] #(conj % {:action :debit
                                                    :account-id (:id pets)
                                                    :amount 13M})))
        updated (transactions/update storage-spec to-update)
        expected-items [{:index 1
                         :amount 12M
                         :balance 25M}
                        {:index 0
                         :amount 13M
                         :balance 13M}]
        actual-items (map #(select-keys % [:index :amount :balance])
                          (transactions/items-by-account storage-spec (:id pets)))]
    (testing "item values are correct"
      (is (= expected-items actual-items)
          "The Pets account should have the correct items"))
    (testing "account balances are correct"
      (is (= 25M
             (:balance (accounts/reload storage-spec pets)))
          "Pets should have the correct balance after update")
      (is (= 281M
             (:balance (accounts/reload storage-spec groceries)))
          "Groceries should have the correct balance after update")
      (is (= 694M
             (:balance (accounts/reload storage-spec checking)))
          "Checking should have the correct balance after update"))))

(def balance-delta-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset }
              {:name "Salary"
               :type :income}
              {:name "Groceries"
               :type :expense}]
   :transactions [{:transaction-date (t/local-date 2016 1 1)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1000M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000M}]}
                  {:transaction-date (t/local-date 2016 1 15)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1001M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1001M}]}
                  {:transaction-date (t/local-date 2016 2 1)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1100M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1100M}]}
                  {:transaction-date (t/local-date 2016 2 15)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1102M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1102M}]}
                  {:transaction-date (t/local-date 2016 3 1)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :amount 1200M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1200M}]}]})

(deftest get-a-balance-delta
  (let [context (serialization/realize storage-spec balance-delta-context)
        [checking
         salary
         groceries] (:accounts context)
        january (transactions/balance-delta storage-spec
                                            (:id salary)
                                            (t/local-date 2016 1 1)
                                            (t/local-date 2016 1 31))
        february (transactions/balance-delta storage-spec
                                            (:id salary)
                                            (t/local-date 2016 2 1)
                                            (t/local-date 2016 2 29))]
    (is (= 2001M january) "The January value is the sum of polarized amounts for the period")
    (is (= 2202M february) "The February value is the sum of the polarized amounts for the period")))

(deftest get-a-balance-as-of
  (let [context (serialization/realize storage-spec balance-delta-context)
        [checking
         salary
         groceries] (:accounts context)
        january (transactions/balance-as-of storage-spec
                                            (:id checking)
                                            (t/local-date 2016 1 31))
        february (transactions/balance-as-of storage-spec
                                            (:id checking)
                                            (t/local-date 2016 2 29))]
    (is (= 2001M january) "The January value is the balance for the last item in the period")
    (is (= 4203M february) "The February value is the balance for the last item in the period")))
