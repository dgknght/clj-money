(ns clj-money.models.transactions-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer [rename-keys]]
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
                                            pprint-diff
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-transaction
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(defn- assert-account-quantities
  [& args]
  (->> args
       (partition 2)
       (map (fn [[account balance]]
              (is (= balance (:quantity (accounts/reload storage-spec account)))
                  (format "%s should have the quantity %s"
                          (:name account)
                          balance))))
       dorun))

(defn items-by-account
  [account-id]
  (transactions/items-by-account
    storage-spec
    account-id
    [(t/local-date 2015 1 1)
     (t/local-date 2017 12 31)]))

(def base-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
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
              :quantity 1000M}
             {:account-id (:id salary)
              :action :credit
              :quantity 1000M}]}))

(deftest create-a-transaction
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create storage-spec (attributes context))]
    (testing "return value includes the new id"
      (is (empty? (validation/error-messages transaction)))
      (is (:id transaction) "A map with the new ID is returned"))
    (testing "transaction can be retrieved"
      (let [actual (-> (transactions/find-by-id storage-spec
                                                (:id transaction)
                                                (:transaction-date transaction))
                       (dissoc :id :created-at :updated-at)
                       (update-in
                         [:items]
                         (partial map #(dissoc % :transaction-id :id :created-at :updated-at))))
            expected {:transaction-date (t/local-date 2016 3 2)
                      :description "Paycheck"
                      :memo "final, partial"
                      :entity-id (-> context :entities first :id)
                      :value 1000M
                      :items [{:description "Paycheck"
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
                               :reconciled? false}]}]
        (pprint-diff expected actual)
        (is (= expected actual)
            "The correct data is retreived")))))

(deftest rollback-on-failure
  (let [call-count (atom 0)]
    (with-redefs [transactions/before-save-item (fn [item]
                                                  (if (= 1 @call-count)
                                                    (throw (RuntimeException. "Induced error"))
                                                    (do
                                                      (swap! call-count inc)
                                                      (update-in item [:action] name))))]
      (let [context (serialization/realize storage-spec base-context)
            [checking
             salary
             groceries] (:accounts context)
            transaction (try
                          (transactions/create storage-spec (attributes context))
                          (catch RuntimeException e
                            nil))]
        (testing "records are not created"
          (is (= 0 (count (transactions/search
                            storage-spec
                            {:entity-id (-> context :entities first :id)
                             :transaction-date "2016"})))
              "The transaction should not be saved")
          (is (= 0 (count (items-by-account (:id checking))))
              "The transaction item for checking should not be created")
          (is (= 0 (count (items-by-account (:id salary))))
              "The transaction item for salary should not be created"))
        (assert-account-quantities checking 0M salary 0M)))))

(deftest create-a-transaction-us-string-date
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (assoc :transaction-date "3/2/2016")))]
    (is (empty? (validation/error-messages transaction)) "The transaction is valid")
    (is (= (t/local-date 2016 3 2) (:transaction-date transaction)) "The transaction date is parsed correctly")))

(deftest create-a-transaction-intl-string-date
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (assoc :transaction-date "2016-03-02")))]
    (is (empty? (validation/error-messages transaction)) "The transaction is valid")
    (is (= (t/local-date 2016 3 2) (:transaction-date transaction)) "The transaction date is parsed correctly")))

(deftest transaction-date-is-required
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (dissoc :transaction-date)))]
    (is (validation/has-error? transaction :transaction-date))))

(deftest entity-id-is-required
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create storage-spec (-> (attributes context)
                                                          (dissoc :entity-id)))]
    (is (validation/has-error? transaction :entity-id))))

(deftest items-are-required
  (let [context (serialization/realize storage-spec base-context)]
    (assert-validation-error :items "Count must be greater than or equal to 1"
                             (transactions/create
                               storage-spec
                               (-> context
                                   attributes
                                   (assoc :items []))))))

(deftest item-account-id-is-required
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(dissoc % :account-id))))]
    (is (validation/has-error? transaction :items))))

(deftest item-quantity-is-required
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(dissoc % :quantity))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-quantity-must-be-greater-than-zero
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(assoc % :quantity -1000M))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-action-is-required
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(dissoc % :action))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest item-action-must-be-debit-or-created
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(assoc % :action :not-valid))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

(deftest sum-of-debits-must-equal-sum-of-credits
  (let [context (serialization/realize storage-spec base-context)
        transaction (transactions/create
                      storage-spec
                      (-> (attributes context)
                          (update-in
                            [:items 0]
                            #(assoc % :quantity 1001M))))]
    (is (validation/has-error? transaction :items) "Validation error should be present")))

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
  (let [context (serialization/realize storage-spec balance-context)
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
  (let [context (serialization/realize storage-spec balance-context)
        [checking-items
         salary-items
         groceries-items] (map #(items-by-account (:id %))
                               (:accounts context))]
    (is (= [1 0] (map :index checking-items)) "The checking transaction items have correct indexes")
    (is (= [0] (map :index salary-items)) "The salary transaction items have the correct indexes")
    (is (= [0] (map :index groceries-items)) "The groceries transaction items have the correct indexes")))

(deftest account-balances-are-set-when-saved
  (let [context (serialization/realize storage-spec balance-context)
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
  (let [context (serialization/realize storage-spec insert-context)
        [checking-items
         salary-items
         groceries-items] (map #(items-by-account (:id %))
                               (:accounts context))]
    (is (= [{:index 2
             :quantity 100M
             :balance 801M}
            {:index 1
             :quantity 99M
             :balance 901M}
            {:index 0
             :quantity 1000M
             :balance 1000M}]
           (map #(select-keys % [:index :quantity :balance]) checking-items))
        "The checking item balances should be correct")
    (is (= [801M 1000M 199M]
           (map #(:quantity (accounts/find-by-id storage-spec (:id %))) (:accounts context)))
        "The checking account has the correct balance")))

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
  (let [context (serialization/realize storage-spec multi-context)
        [checking-items
         salary-items
         groceries-items] (map #(items-by-account (:id %))
                               (:accounts context))
        expected-checking-items [{:index 2 :quantity  100M :balance 1000M}
                                 {:index 1 :quantity 1000M :balance 1100M}
                                 {:index 0 :quantity  100M :balance  100M}]
        actual-checking-items (map #(select-keys % [:index :quantity :balance])
                                   checking-items)]
    (pprint-diff expected-checking-items actual-checking-items)
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
  (let [context (serialization/realize storage-spec delete-context)
        [checking
         salary
         groceries] (:accounts context)
        checking-items-before (items-by-account (:id checking))
        trans (-> context
                  :transactions
                  second)
        _ (transactions/delete storage-spec (:id trans) (:transaction-date trans))
        checking-items-after (items-by-account (:id checking))]
    (testing "transaction item balances are adjusted"
      (let [expected-before [{:index 2 :quantity 102M :balance 797M}
                             {:index 1 :quantity 101M :balance 899M}
                             {:index 0 :quantity 1000M :balance 1000M}]
            actual-before (map #(select-keys % [:index :quantity :balance])
                               checking-items-before)
            expected-after[{:index 1 :quantity 102M :balance 898M}
                           {:index 0 :quantity 1000M :balance 1000M}]
            actual-after (map #(select-keys % [:index :quantity :balance]) checking-items-after)]
        (pprint-diff expected-before actual-before)
        (is (= expected-before actual-before)
            "Checking should have the correct items before delete")
        (pprint-diff expected-after actual-after)
        (is (= expected-after actual-after)
            "Checking should have the correct items after delete")))
    (testing "account balances are adjusted"
      (let [checking-after (accounts/find-by-id storage-spec (:id checking))
            groceries-after (accounts/find-by-id storage-spec (:id groceries))]
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
  (let [context (serialization/realize storage-spec update-context)
        {:keys [id transaction-date]} (find-transaction context (t/local-date 2016 3 2) "Paycheck")]
    (testing "items are not included if not specified"
      (let [transaction (first (transactions/search storage-spec
                                                    {:id id
                                                     :transaction-date transaction-date}))]
        (is transaction "The transaction is retrieved successfully")
        (is (nil? (:items transaction)) "The items are not included")
        (is (= 1000M (:value transaction)) "The correct value is returned")))
    (testing "items are included if specified"
      (let [transaction (first (transactions/search storage-spec
                                                    {:id id
                                                     :transaction-date transaction-date}
                                                    {:include-items? true}))]
        (is transaction "The transaction is retrieved successfully")
        (is (:items transaction) "The items are included")))))

(def search-context
  (merge
    base-context
    {:transactions [{:transaction-date "2016-01-01"
                     :entity-id "Personal"
                     :description "Paycheck"
                     :quantity 160101M
                     :debit-account-id "Checking"
                     :credit-account-id "Salary"}
                    {:transaction-date "2016-06-01"
                     :entity-id "Personal"
                     :description "Paycheck"
                     :quantity 160601M
                     :debit-account-id "Checking"
                     :credit-account-id "Salary"}
                    {:transaction-date "2017-01-01"
                     :entity-id "Personal"
                     :description "Paycheck"
                     :quantity 170101M
                     :debit-account-id "Checking"
                     :credit-account-id "Salary"}
                    {:transaction-date "2017-06-01"
                     :entity-id "Personal"
                     :description "Paycheck"
                     :quantity 170601M
                     :debit-account-id "Checking"
                     :credit-account-id "Salary"}
                    {:transaction-date "2017-06-15"
                     :entity-id "Personal"
                     :description "Paycheck"
                     :quantity 170615
                     :debit-account-id "Checking"
                     :credit-account-id "Salary"}]}))

(deftest search-by-year
  (let [context (serialization/realize storage-spec search-context)
        entity (find-entity context "Personal")
        actual (transactions/search storage-spec {:transaction-date "2016"
                                                  :entity-id (:id entity)})]
    (is (= [(t/local-date 2016 1 1)
            (t/local-date 2016 6 1)]
           (map :transaction-date actual))
        "The transactions from the specified year are returned")))

(deftest search-by-month
  (let [context (serialization/realize storage-spec search-context)
        entity (find-entity context "Personal")
        actual (transactions/search storage-spec {:transaction-date "2017-06"
                                                  :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 1)
            (t/local-date 2017 6 15)]
           (map :transaction-date actual))
        "The transactions from the specified month are returned")))

(deftest search-by-date-string
  (let [context (serialization/realize storage-spec search-context)
        entity (find-entity context "Personal")
        actual (transactions/search storage-spec {:transaction-date "2017-06-01"
                                                  :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 1)] (map :transaction-date actual))
        "The transactions from the specified day are returned")))

(deftest search-by-date
  (let [context (serialization/realize storage-spec search-context)
        entity (find-entity context "Personal")
        actual (transactions/search storage-spec {:transaction-date (t/local-date 2017 6 15)
                                                  :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 15)] (map :transaction-date actual))
        "The transactions from the specified day are returned")))

(deftest search-by-date-vector
  (let [context (serialization/realize storage-spec search-context)
        entity (find-entity context "Personal")
        actual (transactions/search storage-spec {:transaction-date [:between
                                                                     (t/local-date 2017 6 1)
                                                                     (t/local-date 2017 6 30)]
                                                  :entity-id (:id entity)})]
    (is (= [(t/local-date 2017 6 1)
            (t/local-date 2017 6 15)]
           (map :transaction-date actual))
        "The transactions from the specified day are returned")))

(deftest update-a-transaction-change-quantity
  (let [context (serialization/realize storage-spec update-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3] (:transactions context)
        ; Change the 2nd transaction quantity for 101 to 99.99
        updated (-> t2
                    (assoc-in [:items 0 :quantity] 99.99M)
                    (assoc-in [:items 1 :quantity] 99.99M))
        result (transactions/update storage-spec updated)
        expected-checking [{:index 2 :quantity 102M   :balance 798.01M}
                           {:index 1 :quantity 99.99M :balance 900.01M}
                           {:index 0 :quantity 1000M  :balance 1000M}]
        actual-checking (->> (items-by-account (:id checking))
                             (map #(select-keys % [:index :quantity :balance])))
        expected-groceries [{:index 1 :quantity 102M   :balance 201.99M}
                            {:index 0 :quantity 99.99M :balance 99.99M}]
        actual-groceries (->> (items-by-account (:id groceries))
                              (map #(select-keys % [:index :quantity :balance])))]
    (is (empty? (validation/error-messages result))
        "The transaction is updated successfully.")
    (testing "transaction item balances are correct"
      (pprint-diff expected-checking actual-checking)
      (is (= expected-checking actual-checking)
          "Checking items should have the correct values after update")
      (pprint-diff expected-groceries actual-groceries)
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (assert-account-quantities checking 798.01M groceries 201.99M)))

(deftest rollback-a-failed-update
  (let [real-reload transactions/reload
        call-count (atom 0)
        context (serialization/realize storage-spec update-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3] (:transactions context)
        updated (-> t2
                    (assoc-in [:items 0 :quantity] 99.99M)
                    (assoc-in [:items 1 :quantity] 99.99M))
        _ (with-redefs [transactions/reload (fn [storage-spec transaction]
                                              (swap! call-count inc)
                                              (if (= 2 @call-count)
                                                (throw (RuntimeException. "Induced exception"))
                                                (real-reload storage-spec transaction)))]
            (try
              (transactions/update storage-spec updated)
              (catch RuntimeException e nil)))]
    (testing "transaction items are not updated"
      (is (= #{101M} (->> t2
                          (transactions/reload storage-spec)
                          :items
                          (map :quantity)
                          (into #{})))))
    (testing "account balances are not updated"
      (is (= 797M (->> checking
                       (accounts/reload storage-spec)
                       :quantity))
          "The checkout account balance should not be changed")
      (is (= 203M (->> groceries
                       (accounts/reload storage-spec)
                       :quantity))
          "The groceries account balance should not be changed"))))

(deftest update-a-transaction-change-date
  (let [context (serialization/realize storage-spec update-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3] (:transactions context)
        updated (assoc t3 :transaction-date (t/local-date 2016 3 10)
                          :original-transaction-date (:transaction-date t3))
        result (transactions/update storage-spec updated)
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
    (is (empty? (validation/error-messages result))
        "The record is saved successfully")
    (testing "transaction item balances are correct"
      (pprint-diff expected-checking actual-checking)
      (is (= expected-checking actual-checking)
          "Checking items should have the correct values after update")
      (pprint-diff expected-groceries actual-groceries)
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (assert-account-quantities checking 797M groceries 203M)
    (testing "transaction is updated"
      (is (= (t/local-date 2016 3 10)
             (:transaction-date (transactions/reload storage-spec updated)))
          "The transaction should be updated"))))

; TODO: Uncomment this test
#_(deftest update-a-transaction-cross-partition-boundary
  (let [context (serialization/realize storage-spec update-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3] (:transactions context)
        updated (assoc t2 :transaction-date (t/local-date 2016 4 12))
        result (transactions/update storage-spec updated)
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
      (pprint-diff expected-checking actual-checking)
      (is (= expected-checking actual-checking)
          "Checking items should have the correct values after update")
      (is (= expected-groceries actual-groceries)
          "Groceries items should have the correct values after update"))
    (assert-account-quantities checking 1797M groceries 203M)
    (testing "transaction is updated"
      (is (= (t/local-date 2016 4 12)
             (:transaction-date (transactions/reload storage-spec t2)))
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
  (let [context (serialization/realize storage-spec short-circuit-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3 t4] (:transactions context)
        updated (-> t3
                    (rename-keys {:transaction-date :original-transaction-date})
                    (assoc :transaction-date (t/local-date 2016 3 8)))
        update-calls (atom {})]
    (binding [update-item transactions/update-item-index-and-balance]
      (with-redefs [transactions/update-item-index-and-balance (fn [storage item]
                                                                 (swap! update-calls
                                                                        (partial record-update-call item))
                                                                 (update-item storage item))]
        (let [result (transactions/update storage-spec updated)
              expected #{{:index 1
                          :quantity 102M
                          :balance 898M}
                         {:index 2
                          :quantity 101M
                          :balance 797M}}
              actual (get @update-calls (:id checking))]
          (is (empty? (validation/error-messages result))
              "The transaction is saved successfully")
          (testing "the expected transactions are updated"
            (pprint-diff expected actual)
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
  (let [context (serialization/realize storage-spec change-account-context)
        [rent
         groceries] (find-accounts context "Rent" "Groceries")
        t3 (find-transaction context (t/local-date 2016 3 16) "Kroger")
        _ (transactions/update storage-spec (assoc-in t3 [:items 0 :account-id] (:id rent)))
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
      (pprint-diff expected-groceries actual-groceries)
      (is (= expected-groceries actual-groceries)
          "Groceries should have the correct items after update")
      (pprint-diff expected-rent actual-rent)
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
  (let [context (serialization/realize storage-spec change-action-context)
        [checking
         salary
         groceries] (:accounts context)
        [t1 t2 t3 t4] (:transactions context)
        updated (transactions/update storage-spec (-> t3
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
  (let [context (serialization/realize storage-spec add-remove-item-context)
        [checking
         pets
         groceries] (find-accounts context "Checking" "Pets" "Groceries")
        t3 (find-transaction context (t/local-date 2016 3 16) "Kroger")
        to-update (-> t3
                      (assoc-in [:items 0 :quantity] 102M)
                      (assoc-in [:items 0 :value] 102M)
                      (update-in [:items] #(remove (fn [item]
                                                     (= (:account-id item)
                                                        (:id pets)))
                                                   %)))
        updated (transactions/update storage-spec to-update)
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
    (testing "item values are correct"
      (is (= expected-items actual-items)
          "The Pets account should have the correct items"))
    (assert-account-quantities pets 0M groceries 306M checking 694M)))

(deftest update-a-transaction-add-item
  (let [context (serialization/realize storage-spec add-remove-item-context)
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
        updated (transactions/update storage-spec to-update)
        expected-items [{:index 1
                         :quantity 12M
                         :balance 25M}
                        {:index 0
                         :quantity 13M
                         :balance 13M}]
        actual-items (map #(select-keys % [:index :quantity :balance])
                          (items-by-account (:id pets)))]
    (testing "item values are correct"
      (pprint-diff expected-items actual-items)
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
    (is (= 2001M january) "The January value is the sum of polarized quantitys for the period")
    (is (= 2202M february) "The February value is the sum of the polarized quantitys for the period")))

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

(deftest create-multiple-transactions-then-recalculate-balances
  (let [context (serialization/realize storage-spec base-context)
        entity (-> context :entities first)
        [checking
         salary
         groceries] (:accounts context)]
    (transactions/with-delayed-balancing storage-spec (:id entity)
      (transactions/create storage-spec {:entity-id (:id entity)
                                         :transaction-date (t/local-date 2017 1 1)
                                         :description "Paycheck"
                                         :items [{:action :debit
                                                  :account-id (:id checking)
                                                  :quantity 1000M}
                                                 {:action :credit
                                                  :account-id (:id salary)
                                                  :quantity 1000M}]})
      (transactions/create storage-spec {:entity-id (:id entity)
                                         :transaction-date (t/local-date 2017 1 15)
                                         :description "Market Street"
                                         :items [{:action :debit
                                                  :account-id (:id groceries)
                                                  :quantity 100M}
                                                 {:action :credit
                                                  :account-id (:id checking)
                                                  :quantity 100M}]})
      (transactions/create storage-spec {:entity-id (:id entity)
                                         :transaction-date (t/local-date 2017 2 1)
                                         :description "Paycheck"
                                         :items [{:action :debit
                                                  :account-id (:id checking)
                                                  :quantity 1000M}
                                                 {:action :credit
                                                  :account-id (:id salary)
                                                  :quantity 1000M}]})
      (is (= 0M (:quantity (accounts/reload storage-spec checking)))
          "The account balance is not recalculated before the form exits"))
    (is (= 1900M (:quantity (accounts/reload storage-spec checking)))
        "The account balance is recalculated after the form exits")))

(deftest use-simplified-items
  (let [context (serialization/realize storage-spec base-context)
        entity (find-entity context "Personal")
        [checking salary] (find-accounts context "Checking" "Salary")
        trx (transactions/create storage-spec {:entity-id (:id entity)
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
    (is (empty? (validation/error-messages trx)) "The transaction is created successfully")
    (pprint-diff expected-items actual-items)
    (is (= expected-items actual-items) "The items are created correctly")))

(deftest set-account-boundaries
  (let [context (serialization/realize storage-spec base-context)
        entity (find-entity context "Personal")
        [checking
         salary
         groceries] (find-accounts context "Checking" "Salary" "Groceries")
        _ (->> [{:transaction-date (t/local-date 2017 2 27)
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
               (mapv #(transactions/create storage-spec %)))
        [checking
         salary
         groceries] (map #(accounts/reload storage-spec %)
                         [checking salary groceries])]
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
