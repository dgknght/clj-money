(ns clj-money.models.transactions-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [clojure.core.async :refer [chan go-loop <!]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.test]
            [dgknght.app-lib.validation :as v]
            [clj-money.transactions :refer [change-date]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.lots :as lots]
            [clj-money.models.entities :as entities]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.test-context :refer [with-context
                                            *context*
                                            basic-context
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-transaction]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(defn- assert-account-quantities
  [& args]
  {:pre [(even? (count args))]}

  (->> args
       (partition 2)
       (map (fn [[account balance]]
              (is (= balance (:quantity (accounts/reload account)))
                  (format "%s should have the quantity %s"
                          (:name account)
                          balance))))
       dorun))

(defmulti items-by-account type)

(defmethod items-by-account clojure.lang.PersistentVector
  [accounts]
  (map items-by-account accounts))

(defmethod items-by-account java.lang.String
  [account-name]
  (items-by-account (find-account account-name)))

(defmethod items-by-account :default
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

(defn attributes []
  (let [checking (find-account "Checking")
        salary (find-account "Salary")
        entity (find-entity "Personal")]
    {:transaction-date (t/local-date 2016 3 2)
     :description "Paycheck"
     :memo "final, partial"
     :entity-id (:id entity)
     :items [{:account-id (:id checking)
              :action :debit
              :memo "conf # 123"
              :quantity 1000M}
             {:account-id (:id salary)
              :action :credit
              :quantity 1000M}]}))

(deftest create-a-transaction
  (with-context base-context
    (let [transaction (transactions/create (attributes))]
      (is transaction "A non-nil value is returned")
      (testing "return value includes the new id"
        (is (valid? transaction))
        (is (:id transaction) "A map with the new ID is returned"))
      (testing "transaction can be retrieved"
        (let [retrieved (transactions/find transaction)
              expected-items [{:description "Paycheck"
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
          (is (comparable? {:transaction-date (t/local-date 2016 3 2)
                            :description "Paycheck"
                            :memo "final, partial"
                            :value 1000M}
                           retrieved)
              "The correct data is retrieved")
          (is (seq-of-maps-like? expected-items (:items retrieved))
              "The correct items are retrieved")))
      (testing "metadata"
        (is (comparable? {:earliest-transaction-date (t/local-date 2016 3 2)
                          :latest-transaction-date (t/local-date 2016 3 2)}
                         (-> (find-entity "Personal")
                             (entities/find)
                             :settings)))))))

(deftest rollback-on-failure
  (let [call-count (atom 0)]
    (with-redefs [transactions/before-save-item (fn [item]
                                                  (if (= 1 @call-count)
                                                    (throw (RuntimeException. "Induced error"))
                                                    (do
                                                      (swap! call-count inc)
                                                      (update-in item [:action] name))))]
      (with-context base-context
        (let [checking (find-account "Checking")
              salary (find-account "Salary")
              entity (find-entity "Personal")]
          (try
            (transactions/create (attributes))
            (catch RuntimeException _
              nil))
          (testing "records are not created"
            (is (= 0 (count (transactions/search
                              {:entity-id (:id entity)
                               :transaction-date [:between>
                                                  (t/local-date 2016 1 1)
                                                  (t/local-date 2017 1 1)]})))
                "The transaction should not be saved")
            (is (= 0 (count (items-by-account checking)))
                "The transaction item for checking should not be created")
            (is (= 0 (count (items-by-account salary)))
                "The transaction item for salary should not be created"))
          (assert-account-quantities checking 0M salary 0M))))))

(deftest transaction-date-is-required
  (with-context base-context
    (let [transaction (transactions/create (dissoc (attributes)
                                                   :transaction-date))]
      (is (invalid? transaction [:transaction-date] "Transaction date is required")))))

(deftest entity-id-is-required
  (with-context base-context
    (let [result (transactions/create (dissoc (attributes)
                                              :entity-id))]
      (is (invalid? result [:entity-id] "Entity is required")))))

(deftest items-are-required
  (with-context base-context
    (let [result (transactions/create (assoc (attributes) :items []))]
      (is (invalid? result [:items] "Items must contain at least 1 item(s)")))))

(deftest item-account-id-is-required
  (with-context base-context
    (let [transaction (transactions/create
                        (update-in
                          (attributes)
                          [:items 0]
                          #(dissoc % :account-id)))]
      (is (invalid? transaction [:items 0 :account-id] "Account is required")))))

(deftest item-quantity-is-required
  (with-context base-context
    (let [transaction (transactions/create
                        (update-in
                          (attributes)
                          [:items 0]
                          #(dissoc % :quantity)))]
      (is (invalid? transaction [:items 0 :quantity] "Quantity is required")))))

(deftest item-quantity-must-be-greater-than-zero
  (with-context base-context
    (let [transaction (transactions/create
                        (update-in
                          (attributes)
                          [:items 0]
                          #(assoc % :quantity -1000M)))]
      (is (invalid? transaction [:items 0 :quantity] "Quantity cannot be less than zero")))))

(deftest item-action-is-required
  (with-context base-context
    (let [transaction (transactions/create
                        (update-in
                          (attributes)
                          [:items 0]
                          #(dissoc % :action)))]
      (is (invalid? transaction [:items 0 :action] "Action is required")))))

(deftest item-action-must-be-debit-or-credit
  (with-context base-context
    (let [transaction (transactions/create
                        (update-in
                          (attributes)
                          [:items 0]
                          #(assoc % :action :not-valid)))]
      (is (invalid? transaction [:items 0 :action] "Action must be debit or credit")))))

(deftest sum-of-debits-must-equal-sum-of-credits
  (with-context base-context
    (let [transaction (transactions/create
                        (update-in
                          (attributes)
                          [:items 0]
                          #(assoc % :quantity 1001M)))]
      (is (invalid? transaction [:items] "Sum of debits must equal the sum of credits")))))

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
  (with-context balance-context
    (let [[checking-items
           salary-items
           groceries-items] (items-by-account ["Checking"
                                               "Salary"
                                               "Groceries"])]
      ; Transactions are returned with most recent first
      (is (= [900M 1000M]
             (map :balance checking-items))
          "The checking account balances are correct")
      (is (= [1000M] (map :balance salary-items))
          "The salary account balances are correct")
      (is (= [100M] (map :balance groceries-items))
          "The groceries account balances are correct"))))

(deftest item-indexes-are-set-when-saved
  (with-context balance-context
    (let [[checking-items
           salary-items
           groceries-items] (items-by-account ["Checking"
                                               "Salary"
                                               "Groceries"])]
      (is (= [1 0] (map :index checking-items)) "The checking transaction items have correct indexes")
      (is (= [0] (map :index salary-items)) "The salary transaction items have the correct indexes")
      (is (= [0] (map :index groceries-items)) "The groceries transaction items have the correct indexes"))))

(deftest account-balances-are-set-when-saved
  (with-context balance-context
    (let [[checking
           salary
           groceries] (find-accounts "Checking"
                                     "Salary"
                                     "Groceries")]
      (assert-account-quantities
        checking 900M
        salary 1000M
        groceries 100M))))

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
  (with-context insert-context
    (is (seq-of-maps-like? [{:index 2
                             :quantity 100M
                             :balance 801M}
                            {:index 1
                             :quantity 99M
                             :balance 901M}
                            {:index 0
                             :quantity 1000M
                             :balance 1000M}]
                           (items-by-account "Checking"))
        "The checking item balances should be correct")
    (is (= [801M 1000M 199M]
           (map (comp :quantity
                      accounts/find
                      find-account)
                ["Checking" "Salary" "Groceries"]))
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
  (with-context multi-context
    (let [checking-items (items-by-account "Checking")
          expected-checking-items #{{:transaction-date (t/local-date 2016 3 10) :quantity  100M}
                                    {:transaction-date (t/local-date 2016 3 2) :quantity 1000M}
                                    {:transaction-date (t/local-date 2016 3 2) :quantity  100M}}
          actual-checking-items (->> checking-items
                                     (map #(select-keys % [:transaction-date :quantity]))
                                     set)]
      (is (= expected-checking-items
             actual-checking-items)
          "The checking account items are correct"))))

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
  (with-context delete-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          checking-items-before (items-by-account (:id checking))
          trans (find-transaction (t/local-date 2016 3 3) "Kroger")
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
              "Groceries should have the correct balance after delete"))))))

(def delete-trading-transaction-context
  (-> base-context
      (update-in [:accounts] concat [{:name "IRA"
                                      :type :asset
                                      :entity-id "Personal"}])
      (update-in [:commodities] concat [{:name "Apple, Inc."
                                         :symbol "AAPL"
                                         :exchange :nasdaq
                                         :type :stock}])
      (assoc :trades [{:type :buy
                       :commodity-id "AAPL"
                       :account-id "IRA"
                       :shares 100M
                       :value 1000M
                       :trade-date (t/local-date 2015 1 1)}])))

(deftest deleting-trading-transactions-deletes-lots-created-by-the-transaction
  (with-context delete-trading-transaction-context
    (let [[{:keys [transaction lot]}] (:trades *context*)]
      (is lot "The lot is present before deleting the transaction")
      (transactions/delete transaction)
      (is (nil? (lots/find lot)) "The lot is not retreivable after deleting the transaction."))))

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
  (with-context update-context
    (let [{:keys [id transaction-date]} (find-transaction (t/local-date 2016 3 2) "Paycheck")]
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
          (is (:items transaction) "The items are included"))))))

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

(deftest search-by-date
  (with-context search-context
    (let [entity (find-entity "Personal")
          actual (transactions/search {:transaction-date (t/local-date 2017 6 15)
                                       :entity-id (:id entity)})]
      (is (= [(t/local-date 2017 6 15)] (map :transaction-date actual))
          "The transactions from the specified day are returned"))))

(deftest search-by-date-vector
  (with-context search-context
    (let [entity (find-entity "Personal")
          actual (transactions/search {:transaction-date [:between
                                                          (t/local-date 2017 6 1)
                                                          (t/local-date 2017 6 30)]
                                       :entity-id (:id entity)})]
      (is (= [(t/local-date 2017 6 1)
              (t/local-date 2017 6 15)]
             (map :transaction-date actual))
          "The transactions from the specified day are returned"))))

(defn- update-items
  [{:keys [items] :as transaction} change-map]
  (let [indexed-items (index-by :account-id items)
        updated-items (reduce (fn [items [account-id item]]
                                (update-in items [account-id] merge item))
                              indexed-items
                              change-map)]
    (assoc transaction :items (vals updated-items))))

(deftest update-a-transaction-change-quantity
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          result (-> (find-transaction (t/local-date 2016 3 12) "Kroger")
                     (update-items {(:id groceries) {:quantity 99.99M}
                                    (:id checking) {:quantity 99.99M}})
                     transactions/update)]
      (is (valid? result))
      (is (seq-of-maps-like? [{:index 2 :quantity  102.00M :balance   798.01M}
                              {:index 1 :quantity   99.99M :balance   900.01M}
                              {:index 0 :quantity 1000.00M :balance 1000.00M}]
                             (items-by-account (:id checking)))
          "Expected the checking account items to be updated.")
      (is (seq-of-maps-like? [{:index 1 :quantity 102.00M :balance 201.99M}
                              {:index 0 :quantity  99.99M :balance  99.99M}]
                             (items-by-account (:id groceries)))
          "Expected the groceries account items to be updated.")
      (assert-account-quantities checking 798.01M groceries 201.99M))))

(deftest update-a-transaction-change-date
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          trx (find-transaction (t/local-date 2016 3 22) "Kroger")
          result (-> trx
                     (change-date (t/local-date 2016 3 10))
                     transactions/update)]
      (is (valid? result))
      (is (seq-of-maps-like? [{:index 2 :transaction-date (t/local-date 2016 3 12) :quantity 101M  :balance 797M}
                              {:index 1 :transaction-date (t/local-date 2016 3 10) :quantity 102M  :balance 898M}
                              {:index 0 :transaction-date (t/local-date 2016 3 2)  :quantity 1000M :balance 1000M}]
                             (items-by-account (:id checking)))
          "Expected the checking items to be updated")
      (is (seq-of-maps-like? [{:index 1 :transaction-date (t/local-date 2016 3 12) :quantity 101M :balance 203M}
                              {:index 0 :transaction-date (t/local-date 2016 3 10) :quantity 102M :balance 102M}]
                             (items-by-account (:id groceries)))
          "Expected the groceries items to be updated")
      (assert-account-quantities checking 797M groceries 203M)
      (testing "transaction is updated"
        (is (= (t/local-date 2016 3 10)
               (:transaction-date (transactions/reload result)))
            "The transaction should be updated")))))

(def ^:private trading-update-context
  (-> basic-context
      (update-in [:commodities] concat [{:name "Apple, Inc."
                                         :symbol "AAPL"
                                         :type :stock
                                         :exchange :nasdaq}])
      (update-in [:accounts] concat [{:name "IRA"
                                      :entity-id "Personal"
                                      :type :asset}])
      (assoc :trades [{:trade-date (t/local-date 2015 1 1)
                       :type :buy
                       :commodity-id "AAPL"
                       :account-id "IRA"
                       :shares 100M
                       :value 1000M}])))

(deftest update-a-trading-transaction
  (with-context trading-update-context
    ; TODO: are there parts that can be changed?
    (testing "the date and quantiies cannot be updated"
      (let [result (-> (get-in *context* [:trades 0 :transaction])
                                (assoc :transaction-date (t/local-date 2015 2 1))
                                transactions/update)]
        (is (= ["A trading transaction cannot be updated."]
               (v/flat-error-messages result)))))))

(deftest update-a-transaction-cross-partition-boundary
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          trx (find-transaction (t/local-date 2016 3 12) "Kroger")
          result (-> trx
                     (assoc :transaction-date (t/local-date 2016 4 12))
                     transactions/update)]
      (is (valid? result))
      (is (seq-of-maps-like? [{:index 2 :quantity  101M :balance  797M}
                              {:index 1 :quantity  102M :balance  898M}
                              {:index 0 :quantity 1000M :balance 1000M}]
                             (items-by-account (:id checking)))
          "Expected the checking items to be updated")
      (is (seq-of-maps-like? [{:index 1 :quantity 101M :balance 203M}
                              {:index 0 :quantity 102M :balance 102M}]
                             (items-by-account (:id groceries)))
          "Expected the groceries items to be updated")
      (assert-account-quantities checking 797M groceries 203M)
      (testing "transaction is updated"
        (is (= (t/local-date 2016 4 12)
               (:transaction-date (transactions/reload result)))
            "The transaction should be updated")))))

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
  (with-context short-circuit-context
    (let [checking (find-account "Checking")
          trx (find-transaction (t/local-date 2016 3 16) "Kroger")
          updated (change-date trx (t/local-date 2016 3 8))
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
            (assert-account-quantities checking 590M)))))))

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
  (with-context change-account-context
    (let [[rent
           groceries] (find-accounts "Rent" "Groceries")
          result (-> (find-transaction (t/local-date 2016 3 16) "Kroger")
                     (update-items {(:id groceries) {:account-id (:id rent)}})
                     transactions/update)]
      (is (valid? result))
      (is (seq-of-maps-like? [{:index 1
                               :quantity 103M
                               :balance 204M}
                              {:index 0
                               :quantity 101M
                               :balance 101M}]
                             (items-by-account (:id groceries)))
          "Expected the groceries items to be updated")
      (is (seq-of-maps-like? [{:index 0
                               :quantity 102M
                               :balance 102M}]
                             (items-by-account (:id rent)))
          "Expected the rent items to be updated")
      (assert-account-quantities groceries 204M rent 102M))))

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
  (with-context change-action-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          result (-> (find-transaction (t/local-date 2016 3 16) "Kroger")
                     (update-items {(:id groceries) {:action :credit}
                                    (:id checking) {:action :debit}})
                     transactions/update)]
      (is (valid? result))
      (is (seq-of-maps-like? [{:index 2
                               :quantity 101M
                               :balance 192M}
                              {:index 1
                               :quantity 12M
                               :balance 91M}
                              {:index 0
                               :quantity 103M
                               :balance 103M}]
                             (items-by-account (:id groceries)))
          "Expected the groceries items to be updated")
      (assert-account-quantities groceries 192M checking 808M))))

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
  (with-context add-remove-item-context
    (let [[checking
           pets
           groceries] (find-accounts "Checking" "Pets" "Groceries")
          result (-> (find-transaction (t/local-date 2016 3 16) "Kroger")
                     (update-items {(:id groceries) {:quantity 102M
                                                     :value 102M}})
                     (update-in [:items] #(remove (fn [item]
                                                    (= (:account-id item)
                                                       (:id pets)))
                                                  %))
                     transactions/update)
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
      (is (valid? result) (str "Expected the transaction to be valid: " (prn-str (dissoc result :v/explanation))))
      (assert-account-quantities pets 0M groceries 306M checking 694M)
      (is (= expected-items actual-items)
          "The account for the changed item should have the correct items"))))

(deftest update-a-transaction-add-item
  (with-context add-remove-item-context
    (let [[pets
           groceries
           checking] (find-accounts "Pets" "Groceries" "Checking")
          t2 (find-transaction (t/local-date 2016 3 9) "Kroger")
          to-update (-> t2
                        (update-items {(:id groceries) {:quantity 90M
                                                        :value 90M}})
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
      (assert-account-quantities pets 25M groceries 281M checking 694M))))

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
  (with-context balance-delta-context
    (let [salary (accounts/find-by {:name "Salary"})
          january (transactions/balance-delta salary
                                              (t/local-date 2016 1 1)
                                              (t/local-date 2016 1 31))
          february (transactions/balance-delta salary
                                               (t/local-date 2016 2 1)
                                               (t/local-date 2016 2 29))]
      (is (= 2001M january) "The January value is the sum of polarized quantitys for the period")
      (is (= 2202M february) "The February value is the sum of the polarized quantitys for the period"))))

(deftest get-a-balance-as-of
  (with-context balance-delta-context
    (let [checking (accounts/find-by {:name "Checking"})
          january (transactions/balance-as-of checking
                                              (t/local-date 2016 1 31))
          february (transactions/balance-as-of checking
                                               (t/local-date 2016 2 29))]
      (is (= 2001M january) "The January value is the balance for the last item in the period")
      (is (= 4203M february) "The February value is the balance for the last item in the period"))))

(deftest create-multiple-transactions-then-recalculate-balances
  (with-context base-context
    (let [entity (find-entity "Personal")
          [checking
           salary
           groceries] (find-accounts "Checking" "Salary" "Groceries")
          progress-chan (chan)
          progress (atom [])]
      (go-loop [p (<! progress-chan)]
               (when p
                 (swap! progress conj p)
                 (recur (<! progress-chan))))
      (transactions/with-delayed-balancing (:id entity) progress-chan
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
          "The account balance is recalculated after the form exits")
      (is (= [{:total 3
               :completed 0}
              {:total 3
               :completed 1}
              {:total 3
               :completed 2}
              {:total 3
               :completed 3}]
             @progress)
          "The progress is reported during the process"))))

(deftest use-simplified-items
  (with-context base-context
    (let [entity (find-entity "Personal")
          [checking salary] (find-accounts "Checking" "Salary")
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
      (is (= expected-items actual-items) "The items are created correctly"))))

(deftest set-account-boundaries
  (with-context base-context
    (let [entity (find-entity "Personal")
          [checking
           salary
           groceries] (find-accounts "Checking" "Salary" "Groceries")
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
          "The groceries account's latest is the grocery purchase"))))

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
  (with-context existing-reconciliation-context
    (let [transaction (find-transaction (t/local-date 2017 1 1) "Paycheck")
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
      (is (invalid? result1 [:items] "A reconciled quantity cannot be updated"))
      (is (invalid? result2 [:items] "A reconciled quantity cannot be updated")))))

(deftest a-reconciled-transaction-item-cannot-be-deleted
  (with-context existing-reconciliation-context
    (let [[item-id date] (-> *context* :reconciliations first :item-refs first)
          transaction (transactions/find-by-item-id item-id date)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"A transaction with reconciled items cannot be deleted."
                            (transactions/delete transaction))
          "An exception is raised")
      (is (do
            (try
              (transactions/delete transaction)
              (catch clojure.lang.ExceptionInfo _ nil))
            (transactions/find-by-item-id item-id date))
          "The transaction can be retrieved after the delete has been denied"))))
