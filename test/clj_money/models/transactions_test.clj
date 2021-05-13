(ns clj-money.models.transactions-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [clojure.set :refer [rename-keys]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.test-context :refer [realize
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-transaction]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(defn- assert-account-quantities
  [& args]
  (->> args
       (partition 2)
       (map (fn [[account balance]]
              (is (= balance (:quantity (accounts/reload account)))
                  (format "%s should have the quantity %s"
                          (:name account)
                          balance))))
       dorun))

(defn items-by-account
  [account]
  (transactions/items-by-account
   account
   [(t/local-date 2015 1 1)
    (t/local-date 2017 12 31)]))

(def base-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}
              {:name "Groceries"
               :type :expense}]})

(defn attributes
  [context]
  (let [[checking
         salary] (:accounts context)]
    {:transaction-date (t/local-date 2016 3 2)
     :description "Paycheck"
     :memo "final, partial"
     :entity-id (-> context :entities first :id)
     :items [{:account-id (:id checking)
              :action :debit
              :memo "conf # 123"
              :quantity 1000M}
             {:account-id (:id salary)
              :action :credit
              :quantity 1000M}]}))

(deftest create-a-transaction
  (let [context (realize base-context)
        transaction (transactions/create (attributes context))]
    (is transaction "A non-nil value is returned")
    (testing "return value includes the new id"
      (is (valid? transaction))
      (is (:id transaction) "A map with the new ID is returned"))
    (testing "transaction can be retrieved"
      (let [actual (transactions/find transaction)
            expected {:transaction-date (t/local-date 2016 3 2)
                      :description "Paycheck"
                      :memo "final, partial"
                      :entity-id (-> context :entities first :id)
                      :value 1000M}
            expected-items [{:description "Paycheck"
                               :account-id (:id (find-account context "Checking"))
                               :index 0
                               :transaction-date (t/local-date 2016 3 2)
                               :action :debit
                               :negative false
                               :memo "conf # 123"
                               :quantity 1000M
                               :polarized-quantity 1000M
                               :balance 1000M
                               :value 1000M
                               :reconciliation-status nil
                               :reconciliation-id nil
                               :reconciled? false}
                              {:description "Paycheck"
                               :account-id (:id (find-account context "Salary"))
                               :index 0
                               :transaction-date (t/local-date 2016 3 2)
                               :action :credit
                               :negative false
                               :memo nil
                               :quantity 1000M
                               :polarized-quantity 1000M
                               :balance 1000M
                               :value 1000M
                               :reconciliation-status nil
                               :reconciliation-id nil
                               :reconciled? false}]]
        (is (comparable? expected actual "The correct data is retrieved"))
        (doseq [[e a] (partition 2 (interleave expected-items (:items actual)))]
          (is (comparable? e a) "Each item is retrieved correctly"))))))

(deftest rollback-on-failure
  (let [call-count (atom 0)]
    (with-redefs [transactions/before-save-item (fn [item]
                                                  (if (= 1 @call-count)
                                                    (throw (RuntimeException. "Induced error"))
                                                    (do
                                                      (swap! call-count inc)
                                                      (update-in item [:action] name))))]
      (let [context (realize base-context)
            [checking
             salary] (:accounts context)
            _ (try
                (transactions/create (attributes context))
                (catch RuntimeException _
                  nil))]
        (testing "records are not created"
          (is (= 0 (count (transactions/search
                           {:entity-id (-> context :entities first :id)
                            :transaction-date "2016"})))
              "The transaction should not be saved")
          (is (= 0 (count (items-by-account checking)))
              "The transaction item for checking should not be created")
          (is (= 0 (count (items-by-account salary)))
              "The transaction item for salary should not be created"))
        (assert-account-quantities checking 0M salary 0M)))))

(deftest transaction-date-is-required
  (let [context (realize base-context)
        transaction (transactions/create (dissoc (attributes context)
                                                 :transaction-date))]
    (is (invalid? transaction [:transaction-date] "Transaction date is required"))))

(deftest entity-id-is-required
  (let [context (realize base-context)
        result (transactions/create (dissoc (attributes context)
                                                 :entity-id))]
    (is (invalid? result [:entity-id] "Entity is required"))))

(deftest items-are-required
  (let [context (realize base-context)
        result (transactions/create
                              (-> context
                                  attributes
                                  (assoc :items [])))]
    (is (invalid? result [:items] "Items must contain at least 1 item(s)"))))

(deftest item-account-id-is-required
  (let [context (realize base-context)
        transaction (transactions/create
                     (update-in
                      (attributes context)
                      [:items 0]
                      #(dissoc % :account-id)))]
    (is (invalid? transaction [:items 0 :account-id] "Account is required"))))

(deftest item-quantity-is-required
  (let [context (realize base-context)
        transaction (transactions/create
                     (update-in
                      (attributes context)
                      [:items 0]
                      #(dissoc % :quantity)))]
    (is (invalid? transaction [:items 0 :quantity] "Quantity is required"))))

(deftest item-quantity-must-be-greater-than-zero
  (let [context (realize base-context)
        transaction (transactions/create
                     (update-in
                      (attributes context)
                      [:items 0]
                      #(assoc % :quantity -1000M)))]
    (is (invalid? transaction [:items 0 :quantity] "Quantity cannot be less than zero"))))

(deftest item-action-is-required
  (let [context (realize base-context)
        transaction (transactions/create
                     (update-in
                      (attributes context)
                      [:items 0]
                      #(dissoc % :action)))]
    (is (invalid? transaction [:items 0 :action] "Action is required"))))

(deftest item-action-must-be-debit-or-credit
  (let [context (realize base-context)
        transaction (transactions/create
                     (update-in
                      (attributes context)
                      [:items 0]
                      #(assoc % :action :not-valid)))]
    (is (invalid? transaction [:items 0 :action] "Action must be debit or credit"))))

(deftest sum-of-debits-must-equal-sum-of-credits
  (let [context (realize base-context)
        transaction (transactions/create
                     (update-in
                      (attributes context)
                      [:items 0]
                      #(assoc % :quantity 1001M)))]
    (is (invalid? transaction [:items] "Sum of debits must equal the sum of credits"))))

(def balance-context
  (merge base-context
         {:transactions [{:transaction-date (t/local-date 2016 3 2)
                          :entity-id "Personal"
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 1000}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1000}]}
                         {:transaction-date (t/local-date 2016 3 3)
                          :entity-id "Personal"
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 100}
                                  {:action :credit
                                   :account-id "Checking"
                                   :quantity 100}]}]}))

(deftest item-balances-are-set-when-saved
  (let [context (realize balance-context)
        [checking-items
         salary-items
         groceries-items] (map #(items-by-account (:id %))
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
  (let [context (realize balance-context)
        [checking-items
         salary-items
         groceries-items] (map items-by-account
                               (:accounts context))]
    (is (= [1 0] (map :index checking-items)) "The checking transaction items have correct indexes")
    (is (= [0] (map :index salary-items)) "The salary transaction items have the correct indexes")
    (is (= [0] (map :index groceries-items)) "The groceries transaction items have the correct indexes")))

(deftest account-balances-are-set-when-saved
  (let [context (realize balance-context)
        [checking
         salary
         groceries] (find-accounts context "Checking"
                                   "Salary"
                                   "Groceries")]
    (assert-account-quantities checking 900M salary 1000M groceries 100M)))

(def insert-context
  (merge base-context
         {:transactions [{:transaction-date (t/local-date 2016 3 2)
                          :entity-id "Personal"
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 1000}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1000}]}
                         {:transaction-date (t/local-date 2016 3 10)
                          :entity-id "Personal"
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 100}
                                  {:action :credit
                                   :account-id "Checking"
                                   :quantity 100}]}
                         {:transaction-date (t/local-date 2016 3 3)
                          :entity-id "Personal"
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 99}
                                  {:action :credit
                                   :account-id "Checking"
                                   :quantity 99}]}]}))

(deftest insert-transaction-before-the-end
  (let [ctx (realize insert-context)
        checking (find-account ctx "Checking")
        items (items-by-account (:id checking))]
    (is (= [{:index 2
             :quantity 100M
             :balance 801M}
            {:index 1
             :quantity 99M
             :balance 901M}
            {:index 0
             :quantity 1000M
             :balance 1000M}]
           (map #(select-keys % [:index :quantity :balance]) items))
        "The checking item balances should be correct")
    (is (= [801M 1000M 199M]
           (map (comp :quantity
                      accounts/find)
                (:accounts ctx)))
        "The accounts have the correct balances")))

(def multi-context
  (-> base-context
      (update-in [:accounts] #(conj % {:name "Bonus"
                                       :type :income
                                       :entity-id "Personal"
                                       :commodity-id "USD"}))
      (merge {:transactions [{:transaction-date (t/local-date 2016 3 2)
                              :entity-id "Personal"
                              :description "Paycheck"
                              :items [{:action :debit
                                       :account-id "Checking"
                                       :quantity 1000}
                                      {:action :debit
                                       :account-id "Checking"
                                       :quantity 100}
                                      {:action :credit
                                       :account-id "Salary"
                                       :quantity 1000}
                                      {:action :credit
                                       :account-id "Bonus"
                                       :quantity 100}]}
                             {:transaction-date (t/local-date 2016 3 10)
                              :entity-id "Personal"
                              :description "Kroger"
                              :items [{:action :debit
                                       :account-id "Groceries"
                                       :quantity 100}
                                      {:action :credit
                                       :account-id "Checking"
                                       :quantity 100}]}]})))

(deftest create-a-transaction-with-multiple-items-for-one-account
  (let [context (realize multi-context)
        checking (find-account context "Checking")
        checking-items (items-by-account (:id checking))
        expected-checking-items #{{:transaction-date (t/local-date 2016 3 10) :quantity  100M}
                                  {:transaction-date (t/local-date 2016 3 2) :quantity 1000M}
                                  {:transaction-date (t/local-date 2016 3 2) :quantity  100M}}
        actual-checking-items (->> checking-items
                                   (map #(select-keys % [:transaction-date :quantity]))
                                   set)]
    (is (= expected-checking-items
           actual-checking-items)
        "The checking account items are correct")))

(def delete-context
  (merge base-context
         {:transactions [{:transaction-date (t/local-date 2016 3 2)
                          :entity-id "Personal"
                          :description "Paycheck"
                          :items [{:action :debit
                                   :account-id "Checking"
                                   :quantity 1000}
                                  {:action :credit
                                   :account-id "Salary"
                                   :quantity 1000}]}
                         {:transaction-date (t/local-date 2016 3 3)
                          :entity-id "Personal"
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 101}
                                  {:action :credit
                                   :account-id "Checking"
                                   :quantity 101}]}
                         {:transaction-date (t/local-date 2016 3 4)
                          :entity-id "Personal"
                          :description "Kroger"
                          :items [{:action :debit
                                   :account-id "Groceries"
                                   :quantity 102}
                                  {:action :credit
                                   :account-id "Checking"
                                   :quantity 102}]}]}))

(deftest delete-a-transaction
  (let [context (realize delete-context)
        [checking
         _
         groceries] (:accounts context)
        checking-items-before (items-by-account (:id checking))
        trans (-> context
                  :transactions
                  second)
        _ (transactions/delete trans)
        checking-items-after (items-by-account (:id checking))]
    (testing "transaction item balances are adjusted"
      (let [expected-before [{:index 2 :quantity 102M :balance 797M}
                             {:index 1 :quantity 101M :balance 899M}
                             {:index 0 :quantity 1000M :balance 1000M}]
            actual-before (map #(select-keys % [:index :quantity :balance])
                               checking-items-before)
            expected-after [{:index 1 :quantity 102M :balance 898M}
                            {:index 0 :quantity 1000M :balance 1000M}]
            actual-after (map #(select-keys % [:index :quantity :balance]) checking-items-after)]
        (is (= expected-before actual-before)
            "Checking should have the correct items before delete")
        (is (= expected-after actual-after)
            "Checking should have the correct items after delete")))
    (testing "account balances are adjusted"
      (let [checking-after (accounts/find checking)
            groceries-after (accounts/find groceries)]
        (is (= 898M (:quantity checking-after))
            "Checking should have the correct balance after delete")
        (is (= 102M (:quantity groceries-after))
            "Groceries should have the correct balance after delete")))))

(def update-context
  (merge
   base-context
   {:transactions [{:transaction-date (t/local-date 2016 3 2)
                    :entity-id "Personal"
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1000}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1000}]}
                   {:transaction-date (t/local-date 2016 3 12)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 101}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 101}]}
                   {:transaction-date (t/local-date 2016 3 22)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 102}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 102}]}]}))

(deftest get-a-transaction
  (let [context (realize update-context)
        {:keys [id transaction-date]} (find-transaction context (t/local-date 2016 3 2) "Paycheck")]
    (testing "items are not included if not specified"
      (let [transaction (first (transactions/search {:id id
                                                     :transaction-date transaction-date}))]
        (is transaction "The transaction is retrieved successfully")
        (is (nil? (:items transaction)) "The items are not included")
        (is (= 1000M (:value transaction)) "The correct value is returned")))
    (testing "items are included if specified"
      (let [transaction (first (transactions/search {:id id
                                                     :transaction-date transaction-date}
                                                    {:include-items? true}))]
        (is transaction "The transaction is retrieved successfully")
        (is (:items transaction) "The items are included")))))

(def search-context
  (merge
   base-context
   {:transactions [{:transaction-date #local-date "2016-01-01"
                    :entity-id "Personal"
                    :description "Paycheck"
                    :quantity 160101M
                    :debit-account-id "Checking"
                    :credit-account-id "Salary"}
                   {:transaction-date #local-date "2016-06-01"
                    :entity-id "Personal"
                    :description "Paycheck"
                    :quantity 160601M
                    :debit-account-id "Checking"
                    :credit-account-id "Salary"}
                   {:transaction-date #local-date "2017-01-01"
                    :entity-id "Personal"
                    :description "Paycheck"
                    :quantity 170101M
                    :debit-account-id "Checking"
                    :credit-account-id "Salary"}
                   {:transaction-date #local-date "2017-06-01"
                    :entity-id "Personal"
                    :description "Paycheck"
                    :quantity 170601M
                    :debit-account-id "Checking"
                    :credit-account-id "Salary"}
                   {:transaction-date #local-date "2017-06-15"
                    :entity-id "Personal"
                    :description "Paycheck"
                    :quantity 170615
                    :debit-account-id "Checking"
                    :credit-account-id "Salary"}]}))

(deftest search-by-year
  (let [context (realize search-context)
        entity (find-entity context "Personal")
        actual (transactions/search {:transaction-date "2016"
                                     :entity-id (:id entity)})]
    (is (= [(t/local-date 2016 1 1)
            (t/local-date 2016 6 1)]
           (map :transaction-date actual))
        "The transactions from the specified year are returned")))

(deftest search-by-month
  (let [context (realize search-context)
        entity (find-entity context "Personal")
        actual (transactions/search {:transaction-date "2017-06"
                                     :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 1)
            (t/local-date 2017 6 15)]
           (map :transaction-date actual))
        "The transactions from the specified month are returned")))

(deftest search-by-date-string
  (let [context (realize search-context)
        entity (find-entity context "Personal")
        actual (transactions/search {:transaction-date "2017-06-01"
                                     :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 1)] (map :transaction-date actual))
        "The transactions from the specified day are returned")))

(deftest search-by-date
  (let [context (realize search-context)
        entity (find-entity context "Personal")
        actual (transactions/search {:transaction-date (t/local-date 2017 6 15)
                                     :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 15)] (map :transaction-date actual))
        "The transactions from the specified day are returned")))

(deftest search-by-date-vector
  (let [context (realize search-context)
        entity (find-entity context "Personal")
        actual (transactions/search {:transaction-date [:between
                                                        (t/local-date 2017 6 1)
                                                        (t/local-date 2017 6 30)]
                                     :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 1)
            (t/local-date 2017 6 15)]
           (map :transaction-date actual))
        "The transactions from the specified day are returned")))

(deftest update-a-transaction-change-quantity
  (let [context (realize update-context)
        [checking
         _
         groceries] (:accounts context)
        [_ t2] (:transactions context)
        ; Change the 2nd transaction quantity for 101 to 99.99
        updated (-> t2
                    (assoc-in [:items 0 :quantity] 99.99M)
                    (assoc-in [:items 1 :quantity] 99.99M))
        result (transactions/update updated)
        expected-checking [{:index 2 :quantity 102M   :balance 798.01M}
                           {:index 1 :quantity 99.99M :balance 900.01M}
                           {:index 0 :quantity 1000M  :balance 1000M}]
        actual-checking (->> (items-by-account (:id checking))
                             (map #(select-keys % [:index :quantity :balance])))
        expected-groceries [{:index 1 :quantity 102M   :balance 201.99M}
                            {:index 0 :quantity 99.99M :balance 99.99M}]
        actual-groceries (->> (items-by-account (:id groceries))
                              (map #(select-keys % [:index :quantity :balance])))]
    (is (valid? result))
    (testing "transaction item balances are correct"
      (is (= expected-checking actual-checking)
          "Checking items should have the correct values after update")
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (assert-account-quantities checking 798.01M groceries 201.99M)))

(deftest rollback-a-failed-update
  (let [real-reload transactions/reload
        call-count (atom 0)
        context (realize update-context)
        [checking
         _
         groceries] (:accounts context)
        [_ t2] (:transactions context)
        updated (-> t2
                    (assoc-in [:items 0 :quantity] 99.99M)
                    (assoc-in [:items 1 :quantity] 99.99M))
        _ (with-redefs [transactions/reload (fn [transaction]
                                              (swap! call-count inc)
                                              (if (= 2 @call-count)
                                                (throw (RuntimeException. "Induced exception"))
                                                (real-reload transaction)))]
            (try
              (transactions/update updated)
              (catch RuntimeException _ nil)))]
    (testing "transaction items are not updated"
      (is (= #{101M} (->> (:items (transactions/reload t2))
                          (map :quantity)
                          (into #{})))))
    (testing "account balances are not updated"
      (is (= 797M (:quantity (accounts/reload checking)))
          "The checkout account balance should not be changed")
      (is (= 203M (:quantity (accounts/reload groceries)))
          "The groceries account balance should not be changed"))))

(deftest update-a-transaction-change-date
  (let [context (realize update-context)
        [checking
         _
         groceries] (:accounts context)
        [_t1 _t2 t3] (:transactions context)
        updated (assoc t3 :transaction-date (t/local-date 2016 3 10)
                       :original-transaction-date (:transaction-date t3))
        result (transactions/update updated)
        expected-checking [{:index 2 :transaction-date (t/local-date 2016 3 12) :quantity 101M  :balance 797M}
                           {:index 1 :transaction-date (t/local-date 2016 3 10) :quantity 102M  :balance 898M}
                           {:index 0 :transaction-date (t/local-date 2016 3 2)  :quantity 1000M :balance 1000M}]
        actual-checking (->> (:id checking)
                             items-by-account
                             (map #(select-keys % [:index :quantity :balance :transaction-date])))
        expected-groceries [{:index 1 :transaction-date (t/local-date 2016 3 12) :quantity 101M :balance 203M}
                            {:index 0 :transaction-date (t/local-date 2016 3 10) :quantity 102M :balance 102M}]
        actual-groceries (->> (:id groceries)
                              items-by-account
                              (map #(select-keys % [:index :quantity :balance :transaction-date])))]
    (is (valid? result))
    (testing "transaction item balances are correct"
      (is (= expected-checking actual-checking)
          "Checking items should have the correct values after update")
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (assert-account-quantities checking 797M groceries 203M)
    (testing "transaction is updated"
      (is (= (t/local-date 2016 3 10)
             (:transaction-date (transactions/reload updated)))
          "The transaction should be updated"))))

; TODO: Uncomment this test
#_(deftest update-a-transaction-cross-partition-boundary
    (let [context (realize update-context)
          [checking
           salary
           groceries] (:accounts context)
          [t1 t2 t3] (:transactions context)
          updated (assoc t2 :transaction-date (t/local-date 2016 4 12))
          result (transactions/update updated)
          expected-checking [{:index 2 :quantity 101M :balance 797M}
                             {:index 1 :quantity 102M :balance 898M}
                             {:index 0 :quantity 1000M :balance 1000M}]
          actual-checking (->> (:id checking)
                               items-by-account
                               (map #(select-keys % [:index :quantity :balance])))
          expected-groceries [{:index 1 :quantity 101M :balance 203M}
                              {:index 0 :quantity 102M :balance 102M}]
          actual-groceries (->> (:id groceries)
                                items-by-account
                                (map #(select-keys % [:index :quantity :balance])))]
      (is (empty? (validation/error-messages result))
          "The transaction is saved successfully")
      (testing "transaction item balances are correct"
        (is (= expected-checking actual-checking)
            "Checking items should have the correct values after update")
        (is (= expected-groceries actual-groceries)
            "Groceries items should have the correct values after update"))
      (assert-account-quantities checking 1797M groceries 203M)
      (testing "transaction is updated"
        (is (= (t/local-date 2016 4 12)
               (:transaction-date (transactions/reload t2)))
            "The transaction should be updated"))))

(def short-circuit-context
  (merge
   base-context
   {:transactions [{:transaction-date (t/local-date 2016 3 2)
                    :entity-id "Personal"
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1000}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1000}]}
                   {:transaction-date (t/local-date 2016 3 9)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 101}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 101}]}
                   {:transaction-date (t/local-date 2016 3 16)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 102}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 102}]}
                   {:transaction-date (t/local-date 2016 3 23)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 103}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 103}]}
                   {:transaction-date (t/local-date 2016 3 30)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 104}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 104}]}]}))

(defn- record-update-call
  [item result]
  (update-in result
             [(:account-id item)]
             #((fnil conj #{}) % (select-keys item [:index
                                                    :quantity
                                                    :balance]))))
(def ^:dynamic update-item nil)

; Trans. Date quantity  Debit     Credit
; 2016-03-02    1000  Checking  Salary
; 2016-03-09     101  Groceries Checking
; 2016-03-16     102  Groceries Checking move this to 3/8
; 2016-03-23     103  Groceries Checking
; 2016-03-30     104  Groceries Checking
(deftest update-a-transaction-short-circuit-updates
  (let [context (realize short-circuit-context)
        [checking] (:accounts context)
        [_t1 _t2 t3] (:transactions context)
        updated (-> t3
                    (rename-keys {:transaction-date :original-transaction-date})
                    (assoc :transaction-date (t/local-date 2016 3 8)))
        update-calls (atom {})]
    (binding [update-item transactions/update-item-index-and-balance]
      (with-redefs [transactions/update-item-index-and-balance (fn [item]
                                                                 (swap! update-calls
                                                                        (partial record-update-call item))
                                                                 (update-item item))]
        (let [result (transactions/update updated)
              expected #{{:index 1
                          :quantity 102M
                          :balance 898M}
                         {:index 2
                          :quantity 101M
                          :balance 797M}}
              actual (get @update-calls (:id checking))]
          (is (valid? result))
          (testing "the expected transactions are updated"
            (is (= expected actual)
                "Only items with changes are updated")
            (is (not-any? #(= (:index %) 4) actual) "The last item is never updated"))
          (assert-account-quantities checking 590M))))))

(def change-account-context
  (-> base-context
      (update-in [:accounts] #(conj % {:name "Rent"
                                       :type :expense
                                       :commodity-id "USD"}))
      (merge
       {:transactions [{:transaction-date (t/local-date 2016 3 2)
                        :entity-id "Personal"
                        :description "Paycheck"
                        :items [{:action :debit
                                 :account-id "Checking"
                                 :quantity 1000}
                                {:action :credit
                                 :account-id "Salary"
                                 :quantity 1000}]}
                       {:transaction-date (t/local-date 2016 3 9)
                        :entity-id "Personal"
                        :description "Kroger"
                        :items [{:action :debit
                                 :account-id "Groceries"
                                 :quantity 101}
                                {:action :credit
                                 :account-id "Checking"
                                 :quantity 101}]}
                       {:transaction-date (t/local-date 2016 3 16)
                        :entity-id "Personal"
                        :description "Kroger"
                        :items [{:action :debit
                                 :account-id "Groceries"
                                 :quantity 102}
                                {:action :credit
                                 :account-id "Checking"
                                 :quantity 102}]}
                       {:transaction-date (t/local-date 2016 3 23)
                        :entity-id "Personal"
                        :description "Kroger"
                        :items [{:action :debit
                                 :account-id "Groceries"
                                 :quantity 103}
                                {:action :credit
                                 :account-id "Checking"
                                 :quantity 103}]}]})))

(deftest update-a-transaction-change-account
  (let [context (realize change-account-context)
        [rent
         groceries] (find-accounts context "Rent" "Groceries")
        t3 (find-transaction context (t/local-date 2016 3 16) "Kroger")
        _ (transactions/update (assoc-in t3 [:items 0 :account-id] (:id rent)))
        actual-groceries (map #(select-keys % [:index :quantity :balance])
                              (items-by-account (:id groceries)))
        actual-rent (map #(select-keys % [:index :quantity :balance])
                         (items-by-account (:id rent)))
        expected-groceries [{:index 1
                             :quantity 103M
                             :balance 204M}
                            {:index 0
                             :quantity 101M
                             :balance 101M}]
        expected-rent [{:index 0
                        :quantity 102M
                        :balance 102M}]]
    (testing "Accounts have the correct items"
      (is (= expected-groceries actual-groceries)
          "Groceries should have the correct items after update")
      (is (= expected-rent actual-rent)
          "Rent chould have the correct items after update"))
    (assert-account-quantities groceries 204M rent 102M)))

(def change-action-context
  (merge
   base-context
   {:transactions [{:transaction-date (t/local-date 2016 3 2)
                    :entity-id "Personal"
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1000}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1000}]}
                   {:transaction-date (t/local-date 2016 3 9)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 103}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 103}]}
                   {:transaction-date (t/local-date 2016 3 16)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 12}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 12}]}
                   {:transaction-date (t/local-date 2016 3 23)
                    :entity-id "Personal"
                    :description "Kroger"
                    :items [{:action :debit
                             :account-id "Groceries"
                             :quantity 101}
                            {:action :credit
                             :account-id "Checking"
                             :quantity 101}]}]}))

(deftest update-a-transaction-change-action
  (let [context (realize change-action-context)
        [checking _ groceries] (:accounts context)
        transaction (find-transaction context (t/local-date 2016 3 16) "Kroger")
        result (transactions/update (-> transaction
                                        (assoc-in [:items 0 :action] :credit)
                                        (assoc-in [:items 1 :action] :debit)))
        expected-items [{:index 2
                         :quantity 101M
                         :balance 192M}
                        {:index 1
                         :quantity 12M
                         :balance 91M}
                        {:index 0
                         :quantity 103M
                         :balance 103M}]
        actual-items (map #(select-keys % [:index :quantity :balance])
                          (items-by-account (:id groceries)))]
    (is (valid? result))
    (testing "items are updated correctly"
      (is (= expected-items actual-items)
          "Groceries should have the correct items after update"))
    (assert-account-quantities groceries 192M checking 808M)))

(def add-remove-item-context
  (-> base-context
      (update-in [:accounts] #(conj % {:name "Pets"
                                       :type :expense
                                       :commodity-id "USD"}))
      (merge {:transactions [{:transaction-date (t/local-date 2016 3 2)
                              :entity-id "Personal"
                              :description "Paycheck"
                              :items [{:action :debit
                                       :account-id "Checking"
                                       :quantity 1000}
                                      {:action :credit
                                       :account-id "Salary"
                                       :quantity 1000}]}
                             {:transaction-date (t/local-date 2016 3 9)
                              :entity-id "Personal"
                              :description "Kroger"
                              :items [{:action :debit
                                       :account-id "Groceries"
                                       :quantity 103}
                                      {:action :credit
                                       :account-id "Checking"
                                       :quantity 103}]}
                             {:transaction-date (t/local-date 2016 3 16)
                              :entity-id "Personal"
                              :description "Kroger"
                              :items [{:action :debit
                                       :account-id "Groceries"
                                       :quantity 90}
                                      {:action :debit
                                       :account-id "Pets"
                                       :quantity 12}
                                      {:action :credit
                                       :account-id "Checking"
                                       :quantity 102}]}
                             {:transaction-date (t/local-date 2016 3 23)
                              :entity-id "Personal"
                              :description "Kroger"
                              :items [{:action :debit
                                       :account-id "Groceries"
                                       :quantity 101}
                                      {:action :credit
                                       :account-id "Checking"
                                       :quantity 101}]}]})))

(deftest update-a-transaction-remove-item
  (let [context (realize add-remove-item-context)
        [checking
         pets
         groceries] (find-accounts context "Checking" "Pets" "Groceries")
        transaction (find-transaction context (t/local-date 2016 3 16) "Kroger")
        to-update (-> transaction
                      (assoc-in [:items 0 :quantity] 102M)
                      (assoc-in [:items 0 :value] 102M)
                      (update-in [:items] #(remove (fn [item]
                                                     (= (:account-id item)
                                                        (:id pets)))
                                                   %)))
        result (transactions/update to-update)
        expected-items [{:index 2
                         :quantity 101M
                         :balance 306M}
                        {:index 1
                         :quantity 102M
                         :balance 205M}
                        {:index 0
                         :quantity 103M
                         :balance 103M}]
        actual-items (map #(select-keys % [:index :quantity :balance])
                          (items-by-account (:id groceries)))]
    (is (valid? result))
    (assert-account-quantities pets 0M groceries 306M checking 694M)
    (is (= expected-items actual-items)
          "The account for the changed item should have the correct items")))

(deftest update-a-transaction-add-item
  (let [context (realize add-remove-item-context)
        [pets
         groceries
         checking] (find-accounts context "Pets" "Groceries" "Checking")
        t2 (find-transaction context (t/local-date 2016 3 9) "Kroger")
        to-update (-> t2
                      (assoc-in [:items 0 :quantity] 90M)
                      (assoc-in [:items 0 :value] 90M)
                      (update-in [:items] #(conj % {:action :debit
                                                    :account-id (:id pets)
                                                    :quantity 13M
                                                    :value 13M})))
        _ (transactions/update to-update)
        expected-items [{:index 1
                         :quantity 12M
                         :balance 25M}
                        {:index 0
                         :quantity 13M
                         :balance 13M}]
        actual-items (map #(select-keys % [:index :quantity :balance])
                          (items-by-account (:id pets)))]
    (testing "item values are correct"
      (is (= expected-items actual-items)
          "The Pets account should have the correct items"))
    (assert-account-quantities pets 25M groceries 281M checking 694M)))

(def balance-delta-context
  (merge
   base-context
   {:transactions [{:transaction-date (t/local-date 2016 1 1)
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1000M}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1000M}]}
                   {:transaction-date (t/local-date 2016 1 15)
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1001M}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1001M}]}
                   {:transaction-date (t/local-date 2016 2 1)
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1100M}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1100M}]}
                   {:transaction-date (t/local-date 2016 2 15)
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1102M}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1102M}]}
                   {:transaction-date (t/local-date 2016 3 1)
                    :description "Paycheck"
                    :items [{:action :debit
                             :account-id "Checking"
                             :quantity 1200M}
                            {:action :credit
                             :account-id "Salary"
                             :quantity 1200M}]}]}))

(deftest get-a-balance-delta
  (let [context (realize balance-delta-context)
        [_ salary] (:accounts context)
        january (transactions/balance-delta salary
                                            (t/local-date 2016 1 1)
                                            (t/local-date 2016 1 31))
        february (transactions/balance-delta salary
                                             (t/local-date 2016 2 1)
                                             (t/local-date 2016 2 29))]
    (is (= 2001M january) "The January value is the sum of polarized quantitys for the period")
    (is (= 2202M february) "The February value is the sum of the polarized quantitys for the period")))

(deftest get-a-balance-as-of
  (let [context (realize balance-delta-context)
        [checking] (:accounts context)
        january (transactions/balance-as-of checking
                                            (t/local-date 2016 1 31))
        february (transactions/balance-as-of checking
                                             (t/local-date 2016 2 29))]
    (is (= 2001M january) "The January value is the balance for the last item in the period")
    (is (= 4203M february) "The February value is the balance for the last item in the period")))

(deftest create-multiple-transactions-then-recalculate-balances
  (let [context (realize base-context)
        entity (-> context :entities first)
        [checking
         salary
         groceries] (:accounts context)]
    (transactions/with-delayed-balancing (:id entity)
      (transactions/create {:entity-id (:id entity)
                            :transaction-date (t/local-date 2017 1 1)
                            :description "Paycheck"
                            :items [{:action :debit
                                     :account-id (:id checking)
                                     :quantity 1000M}
                                    {:action :credit
                                     :account-id (:id salary)
                                     :quantity 1000M}]})
      (transactions/create {:entity-id (:id entity)
                            :transaction-date (t/local-date 2017 1 15)
                            :description "Market Street"
                            :items [{:action :debit
                                     :account-id (:id groceries)
                                     :quantity 100M}
                                    {:action :credit
                                     :account-id (:id checking)
                                     :quantity 100M}]})
      (transactions/create {:entity-id (:id entity)
                            :transaction-date (t/local-date 2017 2 1)
                            :description "Paycheck"
                            :items [{:action :debit
                                     :account-id (:id checking)
                                     :quantity 1000M}
                                    {:action :credit
                                     :account-id (:id salary)
                                     :quantity 1000M}]})
      (is (= 0M (:quantity (accounts/reload checking)))
          "The account balance is not recalculated before the form exits"))
    (is (= 1900M (:quantity (accounts/reload checking)))
        "The account balance is recalculated after the form exits")))

(deftest use-simplified-items
  (let [context (realize base-context)
        entity (find-entity context "Personal")
        [checking salary] (find-accounts context "Checking" "Salary")
        trx (transactions/create {:entity-id (:id entity)
                                  :transaction-date (t/local-date 2017 3 2)
                                  :description "Paycheck"
                                  :quantity 1000M
                                  :debit-account-id (:id checking)
                                  :credit-account-id (:id salary)})
        actual-items (map #(select-keys % [:account-id :quantity :action]) (:items trx))
        expected-items [{:account-id (:id checking)
                         :action :debit
                         :quantity 1000M}
                        {:account-id (:id salary)
                         :action :credit
                         :quantity 1000M}]]
    (is (valid? trx))
    (is (= expected-items actual-items) "The items are created correctly")))

(deftest set-account-boundaries
  (let [context (realize base-context)
        entity (find-entity context "Personal")
        [checking
         salary
         groceries] (find-accounts context "Checking" "Salary" "Groceries")
        created (->> [{:transaction-date (t/local-date 2017 2 27)
                 :description "Paycheck"
                 :quantity 1000M
                 :debit-account-id (:id checking)
                 :credit-account-id (:id salary)}
                {:transaction-date (t/local-date 2017 3 2)
                 :description "Kroger"
                 :quantity 100M
                 :debit-account-id (:id groceries)
                 :credit-account-id (:id checking)}]
               (map #(assoc % :entity-id (:id entity)))
               (mapv transactions/create))
        [checking
         salary
         groceries] (map accounts/reload [checking salary groceries])]
    (is (valid? created))
    (is (= (t/local-date 2017 2 27) (:earliest-transaction-date checking))
        "The checking account's earliest is the paycheck")
    (is (= (t/local-date 2017 3 2) (:latest-transaction-date checking))
        "The checking account's latest is the grocery purchase")
    (is (= (t/local-date 2017 2 27) (:earliest-transaction-date salary))
        "The salary account's earliest is the paycheck")
    (is (= (t/local-date 2017 2 27) (:latest-transaction-date salary))
        "The salary account's latest is the paycheck")
    (is (= (t/local-date 2017 3 2) (:earliest-transaction-date groceries))
        "The groceries account's earliest is the grocery purchase")
    (is (= (t/local-date 2017 3 2) (:latest-transaction-date groceries))
        "The groceries account's latest is the grocery purchase")))

(def ^:private existing-reconciliation-context
  (-> base-context
      (update-in [:accounts] conj {:name "Rent"
                                   :type :expense
                                   :entity-id "Personal"})
      (assoc :transactions [{:transaction-date (t/local-date 2017 1 1)
                             :description "Paycheck"
                             :debit-account-id "Checking"
                             :credit-account-id "Salary"
                             :quantity 1000M}
                            {:transaction-date (t/local-date 2017 1 2)
                             :description "Landlord"
                             :debit-account-id "Rent"
                             :credit-account-id "Checking"
                             :quantity 500M}
                            {:transaction-date (t/local-date 2017 1 3)
                             :description "Kroger"
                             :debit-account-id "Groceries"
                             :credit-account-id "Checking"
                             :quantity 45M}
                            {:transaction-date (t/local-date 2017 1 10)
                             :description "Safeway"
                             :debit-account-id "Groceries"
                             :credit-account-id "Checking"
                             :quantity 53M}]
             :reconciliations
             [{:account-id "Checking"
               :end-of-period (t/local-date 2017 1 1)
               :balance 1000M
               :status :completed
               :item-refs [{:transaction-date (t/local-date 2017 1 1)
                            :quantity 1000M}]}])))

(deftest the-quantity-and-action-of-a-reconciled-item-cannot-be-changed
  (let [context (realize existing-reconciliation-context)
        transaction (find-transaction context (t/local-date 2017 1 1) "Paycheck")
        result1 (transactions/update (update-in transaction [:items]
                                                #(map (fn [item]
                                                        (assoc item :quantity 1M))
                                                      %)))
        result2 (transactions/update (update-in transaction [:items]
                                                #(map (fn [item]
                                                        (update-in item [:action] (fn [a] (if (= :credit a)
                                                                                            :debit
                                                                                            :credit))))
                                                      %)))]
    (is (invalid? result1 [:items] "A reconciled item cannot be updated"))
    (is (invalid? result2 [:items] "A reconciled item cannot be updated"))))

(deftest a-reconciled-transaction-item-cannot-be-deleted
  (let [context (realize existing-reconciliation-context)
        [item-id date] (-> context :reconciliations first :item-refs first)
        transaction (transactions/find-by-item-id item-id date)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"A transaction with reconciled items cannot be deleted."
                          (transactions/delete transaction))
        "An exception is raised")
    (is (do
          (try
            (transactions/delete transaction)
            (catch clojure.lang.ExceptionInfo _ nil))
          (transactions/find-by-item-id item-id date))
        "The transaction can be retrieved after the delete has been denied")))
