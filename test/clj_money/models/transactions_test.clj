(ns clj-money.models.transactions-test
  (:require [clojure.test :refer [deftest use-fixtures testing is]]
            [clojure.core.async :refer [chan go-loop <!]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.db.sql.ref]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.test_assertions]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]
            [clj-money.accounts :as acts]
            [clj-money.model-helpers :as helpers :refer [assert-invalid]]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.models.transactions :as transactions]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-transaction]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def ^:private reload-account
  (comp models/find
        find-account))

(defn- assert-account-quantities
  [& {:as balances}]
  (doseq [[account balance] balances]
    (is (= balance (:account/quantity (models/find account)))
        (format "%s should have the quantity %s"
                (:account/name account)
                balance))))

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
    :earliest-date (t/local-date 2015 1 1)
    :latest-date (t/local-date 2017 12 31)))

(def base-context
  [(factory :user, {:user/email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:account{:name "Checking"
             :type :asset
             :entity "Personal"}
   #:account{:name "Salary"
             :type :income
             :entity "Personal"}
   #:account{:name "Groceries"
             :type :expense
             :entity "Personal"}])

(defn attributes []
  #:transaction{:transaction-date (t/local-date 2016 3 2)
                :description "Paycheck"
                :memo "final, partial"
                :entity (find-entity "Personal")
                :items [#:transaction-item{:account (find-account "Checking")
                                           :action :debit
                                           :memo "conf # 123"
                                           :quantity 1000M}
                        #:transaction-item{:account (find-account "Salary")
                                           :action :credit
                                           :quantity 1000M
                                           :memo nil}]})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:transaction/entity :transaction-item/account]))

(deftest create-a-transaction
  (with-context base-context
    (let [{:transaction/keys [items]} (assert-created (attributes))
          date (t/local-date 2016 3 2)]
      (testing "entity updates"
        (is (comparable? #:settings{:earliest-transaction-date date
                                    :latest-transaction-date date}
                         (:entity/settings (models/find (find-entity "Personal"))))
            "The entity is updated with the transaction dates"))
      (testing "account updates"
        (is (comparable? #:account{:earliest-transaction-date date
                                   :latest-transaction-date date}
                         (reload-account "Checking"))
            "The debited account is updated with transaction dates")
        (is (comparable? #:account{:earliest-transaction-date date
                                   :latest-transaction-date date}
                         (reload-account "Salary"))
            "The credited account is updated with transaction dates"))
      (testing "item updates"
        (is (comparable? #:transaction-item{:index 0
                                            :balance 1000M}
                         (first items))
            "The first item has the correct summary data")
        (is (comparable? #:transaction-item{:index 0
                                            :balance 1000M}
                         (second items))
            "The first item has the correct summary data")))))

; (deftest rollback-on-failure
;   (let [call-count (atom 0)]
;     (with-redefs [transactions/before-save-item (fn [item]
;                                                   (if (= 1 @call-count)
;                                                     (throw (RuntimeException. "Induced error"))
;                                                     (do
;                                                       (swap! call-count inc)
;                                                       (update-in item [:action] name))))]
;       (with-context base-context
;         (let [checking (find-account "Checking")
;               salary (find-account "Salary")
;               entity (find-entity "Personal")]
;           (try
;             (transactions/create (attributes))
;             (catch RuntimeException _
;               nil))
;           (testing "records are not created"
;             (is (= 0 (count (transactions/search
;                               {:entity-id (:id entity)
;                                :transaction-date [:between>
;                                                   (t/local-date 2016 1 1)
;                                                   (t/local-date 2017 1 1)]})))
;                 "The transaction should not be saved")
;             (is (= 0 (count (items-by-account checking)))
;                 "The transaction item for checking should not be created")
;             (is (= 0 (count (items-by-account salary)))
;                 "The transaction item for salary should not be created"))
;           (assert-account-quantities checking 0M salary 0M))))))

(deftest transaction-date-is-required
  (with-context base-context
    (assert-invalid (dissoc (attributes)
                            :transaction/transaction-date)
                    {:transaction/transaction-date ["Transaction date is required"]})))

(deftest entity-is-required
  (with-context base-context
    (assert-invalid (dissoc (attributes)
                            :transaction/entity)
                    {:transaction/entity ["Entity is required"]})))

(deftest items-are-required
  (with-context base-context
    (assert-invalid (assoc (attributes) :transaction/items [])
                    {:transaction/items ["Items must contain at least 2 item(s)"]})))

(deftest item-account-is-required
  (with-context base-context
    (assert-invalid (update-in
                      (attributes)
                      [:transaction/items 0]
                      dissoc
                      :transaction-item/account)
                    {:transaction/items
                     {0
                      {:transaction-item/account ["Account is required"]}}})))

(deftest item-quantity-is-required
  (with-context base-context
    (assert-invalid (update-in (attributes)
                               [:transaction/items 0]
                               #(-> %
                                    (dissoc :transaction-item/quantity)
                                    (assoc :transaction-item/value 1M)))
                    {:transaction/items
                     {0
                      {:transaction-item/quantity ["Quantity is required"]}}})))

(deftest item-quantity-must-be-greater-than-zero
  (with-context base-context
    (assert-invalid (assoc-in
                          (attributes)
                          [:transaction/items
                           0
                           :transaction-item/quantity]
                          -1000M)
                    {:transaction/items
                     {0
                      {:transaction-item/quantity ["Quantity is invalid"] ; TODO: Adjust this message to say "Quantity must be a positive number"
                       :transaction-item/value ["Value is invalid"]}}})))

(deftest item-action-is-required
  (with-context base-context
    (assert-invalid (update-in
                          (attributes)
                          [:transaction/items 0]
                          #(dissoc % :transaction-item/action))
                    {:transaction/items
                     {0
                      {:transaction-item/action ["Action is required"]}}})))

(deftest item-action-must-be-debit-or-credit
  (with-context base-context
    (assert-invalid (assoc-in
                      (attributes)
                      [:transaction/items
                       0
                       :transaction-item/action]
                      :not-valid)
                    {:transaction/items
                     {0
                      {:transaction-item/action ["Action must be debit or credit"]}}})))

(deftest sum-of-debits-must-equal-sum-of-credits
  (with-context base-context
    (assert-invalid (assoc-in (attributes)
                              [:transaction/items
                               0
                               :transaction-item/quantity]
                              1001M)
                    {:transaction/items ["Sum of debits must equal the sum of credits"]})
    (assert-invalid (assoc-in (attributes)
                              [:transaction/items
                               0
                               :transaction-item/action]
                              :credit)
                    {:transaction/items ["Sum of debits must equal the sum of credits"]})))

(def balance-context
  (conj base-context
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 3)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 100M}))

(deftest item-balances-are-set-when-saved
  (with-context balance-context
    (let [[checking-items
           salary-items
           groceries-items] (items-by-account ["Checking"
                                               "Salary"
                                               "Groceries"])]
      ; Transactions are returned with most recent first
      (is (= [900M 1000M]
             (map :transaction-item/balance checking-items))
          "The checking account balances are correct")
      (is (= [1000M] (map :transaction-item/balance salary-items))
          "The salary account balances are correct")
      (is (= [100M] (map :transaction-item/balance groceries-items))
          "The groceries account balances are correct"))))

(deftest item-indexes-are-set-when-saved
  (with-context balance-context
    (let [[checking-items
           salary-items
           groceries-items] (items-by-account ["Checking"
                                               "Salary"
                                               "Groceries"])]
      (is (= [1 0] (map :transaction-item/index checking-items)) "The checking transaction items have correct indexes")
      (is (= [0]   (map :transaction-item/index salary-items)) "The salary transaction items have the correct indexes")
      (is (= [0]   (map :transaction-item/index groceries-items)) "The groceries transaction items have the correct indexes"))))

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
  (conj base-context
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 10)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 100M}
        #:transaction{:transaction-date (t/local-date 2016 3 3)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 99M}))

(deftest insert-transaction-before-the-end
  (with-context insert-context
    (is (seq-of-maps-like? [#:transaction-item{:index 2
                                               :quantity 100M
                                               :balance 801M}
                            #:transaction-item{:index 1
                                               :quantity 99M
                                               :balance 901M}
                            #:transaction-item{:index 0
                                               :quantity 1000M
                                               :balance 1000M}]
                           (items-by-account "Checking"))
        "The checking item indexes and balances are adjusted")
    (is (= 801M (:account/quantity (reload-account "Checking")))
        "The checking account quantity is updated")
    (is (= 199M (:account/quantity (reload-account "Groceries")))
        "The groceries account quantity is updated")))
 
(def multi-context
  (conj base-context
        #:account{:name "Bonus"
                  :type :income
                  :entity "Personal"
                  :commodity "USD"}
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :items [#:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 1000M}
                              #:transaction-item{:action :debit
                                                 :account "Checking"
                                                 :quantity 100M}
                              #:transaction-item{:action :credit
                                                 :account "Salary"
                                                 :quantity 1000M}
                              #:transaction-item{:action :credit
                                                 :account "Bonus"
                                                 :quantity 100M}]}
        #:transaction{:transaction-date (t/local-date 2016 3 10)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 100M}))

(deftest create-a-transaction-with-multiple-items-for-one-account
  (with-context multi-context
    (let [checking (reload-account "Checking")]
      (is (comparable? {:account/earliest-transaction-date (t/local-date 2016 3 2)
                        :account/latest-transaction-date (t/local-date 2016 3 10)}
                       checking)
          "The checking account transaction date boundaries reflect all transactions")
      (is (seq-of-maps-like? [{:transaction-item/index 0
                               :transaction-item/action :debit
                               :transaction-item/quantity 1000M
                               :transaction-item/balance 1000M}
                              {:transaction-item/index 1
                               :transaction-item/action :debit
                               :transaction-item/quantity 100M
                               :transaction-item/balance 1100M}
                              {:transaction-item/index 2
                               :transaction-item/action :credit
                               :transaction-item/quantity 100M
                               :transaction-item/balance 1000M}]
                             (-> checking
                                 acts/->criteria
                                 (models/select {:sort [[:transaction-item/index :asc]]})))
          "The checking account items has sequential indices and a running balance"))))

(def delete-context
  (conj base-context
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 3)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 100M}
        #:transaction{:transaction-date (t/local-date 2016 3 4)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 102M}))

(deftest delete-a-transaction
  (with-context delete-context
    (let [checking-items-before (items-by-account "Checking")
          trans (find-transaction [(t/local-date 2016 3 3) "Kroger"])
          _ (models/delete trans)
          checking-items-after (items-by-account "Checking")]
      (testing "checking transaction item balances are adjusted"
        (is (seq-of-maps-like? [#:transaction-item{:index 2 :quantity 102M :balance 798M}
                                #:transaction-item{:index 1 :quantity 100M :balance 900M}
                                #:transaction-item{:index 0 :quantity 1000M :balance 1000M}]
                               checking-items-before)
            "The item to be deleted is present before the delete")
        (is (seq-of-maps-like? [#:transaction-item{:index 1 :quantity 102M :balance 898M}
                                #:transaction-item{:index 0 :quantity 1000M :balance 1000M}]
                               checking-items-after)
            "The deleted item is absent after the delete"))
      (testing "account balances are adjusted"
        (is (= 898M (:account/quantity (reload-account "Checking")))
            "The amount has been restored in the checking account")
        (is (= 102M (:account/quantity (reload-account "Groceries")))
            "The amount has been subscracted from the groceries account")))))

(def update-context
  (conj base-context
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 12)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 101M}
        #:transaction{:transaction-date (t/local-date 2016 3 22)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 102M}))

(deftest get-a-transaction
  (with-context update-context
    (let [trx (find-transaction [(t/local-date 2016 3 2) "Paycheck"])]
      (testing "items are not included if not specified"
        (let [retrieved (models/find-by (select-keys trx [:id :transaction/transaction-date]))]
          (is retrieved "a value is returned")
          (is (= 1000M (:transaction/value retrieved))
              "The transaction value can be retrieved")
          (is (= 2 (count (:transaction/items retrieved)))
              "The transaction items are included"))))))

(def search-context
  (conj base-context
        #:transaction{:transaction-date #local-date "2016-01-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 160101M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #local-date "2016-06-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 160601M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #local-date "2017-01-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 170101M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #local-date "2017-06-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 170601M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #local-date "2017-06-15"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 170615M
                      :debit-account "Checking"
                      :credit-account "Salary"}))

(deftest search-by-date
  (with-context search-context
    (is (seq-of-maps-like? [#:transaction{:transaction-date (t/local-date 2017 6 15)
                                          :description "Paycheck"
                                          :value 170615M}]
                           (models/select #:transaction{:transaction-date (t/local-date 2017 6 15)
                                                        :entity (find-entity "Personal")}))
        "The transactions from the specified day are returned")))

(deftest search-by-date-vector
  (with-context search-context
    (is (seq-of-maps-like? [#:transaction{:transaction-date (t/local-date 2017 6 1)}
                            #:transaction{:transaction-date (t/local-date 2017 6 15)}]
                           (models/select #:transaction{:transaction-date [:between
                                                                           (t/local-date 2017 6 1)
                                                                           (t/local-date 2017 6 30)]
                                                        :entity (find-entity "Personal")}))
        "The transactions from the specified day are returned")))

(defn- update-items
  [items change-map]
  (let [indexed-items (index-by (comp util/->model-ref
                                      :transaction-item/account)
                                items)]
    (->> change-map
         (map #(update-in % [0] util/->model-ref))
         (reduce (fn [items [account item]]
                   (update-in items [account] merge item))
                 indexed-items)
         vals
         (into []))))

(defn- update-trx-items
  [trx & {:as change-map}]
  (update-in trx [:transaction/items] update-items change-map))

(deftest update-a-transaction-change-quantity
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")]
      (-> (find-transaction [(t/local-date 2016 3 12) "Kroger"])
          (update-trx-items groceries {:transaction-item/quantity 99.99M}
                            checking {:transaction-item/quantity 99.99M})
          models/put)
      (is (seq-of-maps-like? [#:transaction-item{:index 2 :quantity  102.00M :balance   798.01M}
                              #:transaction-item{:index 1 :quantity   99.99M :balance   900.01M}
                              #:transaction-item{:index 0 :quantity 1000.00M :balance 1000.00M}]
                             (items-by-account checking))
          "Expected the checking account items to be updated.")
      (is (seq-of-maps-like? [#:transaction-item{:index 1 :quantity 102.00M :balance 201.99M}
                              #:transaction-item{:index 0 :quantity  99.99M :balance  99.99M}]
                             (items-by-account groceries))
          "Expected the groceries account items to be updated.")
      (assert-account-quantities checking 798.01M groceries 201.99M))))

(deftest update-a-transaction-change-date
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          trx (find-transaction [(t/local-date 2016 3 22) "Kroger"])
          result (-> trx
                     (assoc :transaction/transaction-date (t/local-date 2016 3 10))
                     models/put)]
      (is (seq-of-maps-like? [#:transaction-item{:index 2
                                                 :transaction-date (t/local-date 2016 3 12)
                                                 :quantity 101M
                                                 :balance 797M}
                              #:transaction-item{:index 1
                                                 :transaction-date (t/local-date 2016 3 10)
                                                 :quantity 102M
                                                 :balance 898M}
                              #:transaction-item{:index 0
                                                 :transaction-date (t/local-date 2016 3 2)
                                                 :quantity 1000M
                                                 :balance 1000M}]
                             (items-by-account checking))
          "The checking account items are updated")
      (is (seq-of-maps-like? [#:transaction-item{:index 1
                                                 :transaction-date (t/local-date 2016 3 12)
                                                 :quantity 101M
                                                 :balance 203M}
                              #:transaction-item{:index 0
                                                 :transaction-date (t/local-date 2016 3 10)
                                                 :quantity 102M
                                                 :balance 102M}]
                             (items-by-account groceries))
          "The groceries account items are updated")
      (assert-account-quantities checking 797M groceries 203M)
      (testing "transaction is updated"
        (is (= (t/local-date 2016 3 10)
               (:transaction/transaction-date (models/find result)))
            "The updated transaction can be retrieved")))))

(deftest update-a-transaction-cross-partition-boundary
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          result (-> (find-transaction [(t/local-date 2016 3 12) "Kroger"])
                     (assoc :transaction/transaction-date (t/local-date 2016 4 12))
                     models/put)]
      (is (seq-of-maps-like? [#:transaction-item{:index 2
                                                 :transaction-date (t/local-date 2016 4 12)
                                                 :quantity 101M
                                                 :balance 797M}
                              #:transaction-item{:index 1
                                                 :transaction-date (t/local-date 2016 3 22)
                                                 :quantity 102M
                                                 :balance 898M}
                              #:transaction-item{:index 0
                                                 :transaction-date (t/local-date 2016 3 2)
                                                 :quantity 1000M
                                                 :balance 1000M}]
                             (items-by-account checking))
          "The checking account items reflect the change in transaction date")
      (is (seq-of-maps-like? [#:transaction-item{:index 1
                                                 :transaction-date (t/local-date 2016 4 12)
                                                 :quantity 101M
                                                 :balance 203M}
                              #:transaction-item{:index 0
                                                 :transaction-date (t/local-date 2016 3 22)
                                                 :quantity 102M
                                                 :balance 102M}]
                             (items-by-account groceries))
          "The groceries account items reflect the change in transaction date")
      (assert-account-quantities checking 797M groceries 203M)
      (testing "transaction is updated"
        (is (= (t/local-date 2016 4 12)
               (:transaction/transaction-date (models/find result)))
            "The retrieved transaction has the new date")))))

(def short-circuit-context
  (conj base-context
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 9)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 101M}
        #:transaction{:transaction-date (t/local-date 2016 3 16)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 102M}
        #:transaction{:transaction-date (t/local-date 2016 3 23)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 103M}
        #:transaction{:transaction-date (t/local-date 2016 3 30)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 104M}))

; Trans. Date quantity  Debit     Credit
; 2016-03-02    1000  Checking  Salary
; 2016-03-09     101  Groceries Checking
; 2016-03-16     102  Groceries Checking move this to 3/8
; 2016-03-23     103  Groceries Checking
; 2016-03-30     104  Groceries Checking
(deftest update-a-transaction-short-circuit-updates
  (with-context short-circuit-context
    (let [calls (atom [])
          orig-put sql/put*]
      (with-redefs [sql/put* (fn [ds models]
                               (swap! calls conj models)
                               (orig-put ds models))]
        (-> (find-transaction [(t/local-date 2016 3 16) "Kroger"])
            (assoc :transaction/transaction-date (t/local-date 2016 3 8))
            models/put)
        (let [[c :as cs] @calls]
          (is (= 1 (count cs))
              "One call is made to write to storage")
          (is (seq-of-maps-like? [#:transaction{:description "Kroger"
                                                :transaction-date (t/local-date 2016 3 8)}]
                                 (filter (db/model-type? :transaction)
                                         c))
              "The updated transaction is written")
          (is (seq-of-maps-like? [#:transaction-item {:index 1
                                                      :quantity 101M
                                                      :balance 203M}
                                  #:transaction-item{:index 2
                                                     :quantity 101M
                                                     :balance 797M}]
                                 (filter (db/model-type? :transaction-item)
                                         c))
              "The affected transaction items are written")

          (is (empty? (filter #(#{3 4} (:transaction-item/index %))
                              c))
              "The unaffected transaction items are not written")
          (is (empty? (filter (db/model-type? :account)
                              c))
              "The account is not updated")
          (assert-account-quantities (find-account "Checking") 590M))))))

(def change-account-context
  (conj base-context
        #:account{:name "Rent"
                  :type :expense
                  :entity "Personal"
                  :commodity "USD"}
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 9)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 101M}
        #:transaction{:transaction-date (t/local-date 2016 3 16)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 102M}
        #:transaction{:transaction-date (t/local-date 2016 3 23)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 103M}))

(deftest update-a-transaction-change-account
  (with-context change-account-context
    (let [[rent
           groceries] (find-accounts "Rent" "Groceries")]
      (-> (find-transaction [(t/local-date 2016 3 16) "Kroger"])
          (update-trx-items groceries {:transaction-item/account rent})
          models/put)
      (is (seq-of-maps-like? [#:transaction-item{:index 1
                                                 :quantity 103M
                                                 :balance 204M}
                              #:transaction-item{:index 0
                                                 :quantity 101M
                                                 :balance 101M}]
                             (items-by-account groceries))
          "The items in the removed account reflect the removal")
      (is (seq-of-maps-like? [#:transaction-item{:index 0
                                                 :quantity 102M
                                                 :balance 102M}]
                             (items-by-account rent))
          "The items in the added account reflect the addition")
      (assert-account-quantities groceries 204M rent 102M))))

(def change-action-context
  (conj base-context
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 9)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 103M}
        #:transaction{:transaction-date (t/local-date 2016 3 16)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 12M}
        #:transaction{:transaction-date (t/local-date 2016 3 23)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 101M}))

(deftest update-a-transaction-change-action
  (with-context change-action-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")]
      (-> (find-transaction [(t/local-date 2016 3 16) "Kroger"])
          (update-trx-items groceries {:transaction-item/action :credit}
                            checking {:transaction-item/action :debit})
          models/put)
      (is (= [#:transaction-item{:index 2
                                 :quantity 101M
                                 :action :debit
                                 :balance 192M}
              #:transaction-item{:index 1
                                 :quantity 12M
                                 :action :credit
                                 :balance 91M}
              #:transaction-item{:index 0
                                 :quantity 103M
                                 :action :debit
                                 :balance 103M}]
             (map #(select-keys % [:transaction-item/index
                                   :transaction-item/action
                                   :transaction-item/quantity
                                   :transaction-item/balance])
                  (items-by-account groceries)))
          "The groceries balances reflect the change in action")
      (is (= [#:transaction-item{:index 3
                                 :action :credit
                                 :quantity 101M
                                 :balance 808M}
              #:transaction-item{:index 2
                                 :action :debit
                                 :quantity 12M
                                 :balance 909M}
              #:transaction-item{:index 1
                                 :action :credit
                                 :quantity 103M
                                 :balance 897M}
              #:transaction-item{:index 0
                                 :action :debit
                                 :quantity 1000M
                                 :balance 1000M}]
             (map #(select-keys % [:transaction-item/index
                                   :transaction-item/action
                                   :transaction-item/quantity
                                   :transaction-item/balance])
                  (items-by-account checking)))
          "The checking balances reflect the change in action")
      (assert-account-quantities groceries 192M checking 808M))))

(def add-remove-item-context
  (conj base-context
        #:account{:name "Pets"
                  :entity "Personal"
                  :type :expense
                  :commodity-id "USD"}
        #:transaction{:transaction-date (t/local-date 2016 3 2)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 3 9)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 103M}
        #:transaction{:transaction-date (t/local-date 2016 3 16)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:action :debit
                                                 :account "Groceries"
                                                 :quantity 90M}
                              #:transaction-item{:action :debit
                                                 :account "Pets"
                                                 :quantity 12M}
                              #:transaction-item{:action :credit
                                                 :account "Checking"
                                                 :quantity 102M}]}
        #:transaction{:transaction-date (t/local-date 2016 3 23)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 101M}))

(deftest update-a-transaction-remove-item
  (with-context add-remove-item-context
    (-> (find-transaction [(t/local-date 2016 3 16) "Kroger"])
        (update-trx-items (find-account "Groceries")
                          #:transaction-item{:quantity 102M
                                             :value 102M})
        (update-in [:transaction/items] #(remove (fn [i]
                                                   (= 12M (:transaction-item/quantity i)))
                                                 %))
        models/put)
    (is (seq-of-maps-like? [#:transaction-item{:index 2
                                               :quantity 101M
                                               :balance 306M}
                            #:transaction-item{:index 1
                                               :quantity 102M
                                               :balance 205M}
                            #:transaction-item{:index 0
                                               :quantity 103M
                                               :balance 103M}]
                           (items-by-account "Groceries")))
    (assert-account-quantities (find-account "Pets")        0M
                               (find-account "Groceries") 306M
                               (find-account "Checking")  694M)))

(deftest update-a-transaction-add-item
  (with-context add-remove-item-context
    (let [[pets
           groceries
           checking] (find-accounts "Pets" "Groceries" "Checking")]
      (-> (find-transaction [(t/local-date 2016 3 9) "Kroger"])
          (update-trx-items groceries #:transaction-item{:quantity 90M
                                                         :value 90M})
          (update-in [:transaction/items]
                     conj
                     #:transaction-item{:action :debit
                                        :account pets
                                        :quantity 13M
                                        :value 13M})
          models/put)
      (testing "item values are correct"
        (is (seq-of-maps-like? [#:transaction-item{:index 1
                                                   :quantity 12M
                                                   :balance 25M}
                                #:transaction-item{:index 0
                                                   :quantity 13M
                                                   :balance 13M}]
                               (items-by-account "Pets"))
            "The Pets account should have the correct items"))
      (assert-account-quantities pets 25M
                                 groceries 281M
                                 checking 694M))))

(def balance-delta-context
  (conj base-context
        #:transaction{:transaction-date (t/local-date 2016 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2016 1 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1001M}
        #:transaction{:transaction-date (t/local-date 2016 2 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1100M}
        #:transaction{:transaction-date (t/local-date 2016 2 15)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1102M}
        #:transaction{:transaction-date (t/local-date 2016 3 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1200M}))

(deftest get-a-balance-delta
  (with-context balance-delta-context
    (let [salary (reload-account "Salary")
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
    (let [checking (reload-account "Checking")]
      (is (= 2001M
             (transactions/balance-as-of checking
                                         (t/local-date 2016 1 31)))
          "The January value is the balance for the last item in the period")
      (is (= 4203M
             (transactions/balance-as-of checking
                                         (t/local-date 2016 2 29)))
          "The February value is the balance for the last item in the period"))))

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
      (transactions/with-delayed-balancing [progress-chan]
        (mapv (comp models/put
                    #(assoc % :transaction/entity entity))
              [#:transaction{:transaction-date (t/local-date 2017 1 1)
                             :description "Paycheck"
                             :debit-account checking
                             :credit-account salary
                             :quantity 1000M}
               #:transaction{:transaction-date (t/local-date 2017 1 15)
                             :description "Market Street"
                             :debit-account groceries
                             :credit-account checking
                             :quantity 100M}
               #:transaction{:transaction-date (t/local-date 2017 2 1)
                             :description "Paycheck"
                             :debit-account checking
                             :credit-account salary
                             :quantity 1000M}])
        (is (= 0M (:account/quantity (reload-account "Checking")))
            "The account balance is not recalculated before the form exits"))
      (is (= 1900M (:account/quantity (reload-account "Checking")))
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
          trx (models/put #:transaction{:entity entity
                                        :transaction-date (t/local-date 2017 3 2)
                                        :description "Paycheck"
                                        :quantity 1000M
                                        :debit-account checking
                                        :credit-account salary})]
      (is (seq-of-maps-like? [#:transaction-item{:quantity 1000M
                                                 :action :debit
                                                 :account (util/->model-ref checking)}
                              #:transaction-item{:quantity 1000M
                                                 :action :credit
                                                 :account (util/->model-ref salary)}]
                             (:transaction/items trx))))))

(deftest set-account-boundaries
  (with-context base-context
    (let [entity (find-entity "Personal")
          [checking
           salary
           groceries] (find-accounts "Checking" "Salary" "Groceries")]
      (->> [#:transaction{:transaction-date (t/local-date 2017 2 27)
                          :description "Paycheck"
                          :quantity 1000M
                          :debit-account checking
                          :credit-account salary}
            #:transaction{:transaction-date (t/local-date 2017 3 2)
                          :description "Kroger"
                          :quantity 100M
                          :debit-account groceries
                          :credit-account checking}]
           (map #(assoc % :transaction/entity entity))
           (mapv models/put))
      (is (comparable? #:account {:earliest-transaction-date (t/local-date 2017 2 27)
                                  :latest-transaction-date (t/local-date 2017 3 2)}
                       (reload-account "Checking")))
      (is (comparable? #:account {:earliest-transaction-date (t/local-date 2017 2 27)
                                  :latest-transaction-date (t/local-date 2017 2 27)}
                       (reload-account "Salary")))
      (is (comparable? #:account {:earliest-transaction-date (t/local-date 2017 3 2)
                                  :latest-transaction-date (t/local-date 2017 3 2)}
                       (reload-account "Groceries"))))))

(def ^:private existing-reconciliation-context
  (conj base-context
        #:account{:name "Rent"
                  :type :expense
                  :entity "Personal"}
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:transaction{:transaction-date (t/local-date 2017 1 2)
                      :entity "Personal"
                      :description "Landlord"
                      :debit-account "Rent"
                      :credit-account "Checking"
                      :quantity 500M}
        #:transaction{:transaction-date (t/local-date 2017 1 3)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 45M}
        #:transaction{:transaction-date (t/local-date 2017 1 10)
                      :entity "Personal"
                      :description "Safeway"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 53M}
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 1)
                         :balance 1000M
                         :status :completed
                         :item-refs [[(t/local-date 2017 1 1)
                                      1000M]]}))

(deftest the-quantity-of-a-reconciled-item-cannot-be-changed
  (with-context existing-reconciliation-context
    (-> (find-transaction [(t/local-date 2017 1 1) "Paycheck"])
        (assoc-in [:transaction/items 0 :transaction-item/quantity] 1010M)
        (assoc-in [:transaction/items 1 :transaction-item/quantity] 1010M)
        (assert-invalid {:transaction/items ["A reconciled quantity cannot be updated"]}))))

(deftest the-action-of-a-reconciled-item-cannot-be-changed
  (with-context existing-reconciliation-context
    (-> (find-transaction [(t/local-date 2017 1 1) "Paycheck"])
        (assoc-in [:transaction/items 0 :transaction-item/action] :credit)
        (assoc-in [:transaction/items 1 :transaction-item/action] :debit)
        (assert-invalid {:transaction/items ["A reconciled quantity cannot be updated"]}))))

(deftest a-reconciled-transaction-item-cannot-be-deleted
  (with-context existing-reconciliation-context
    (let [transaction (find-transaction [(t/local-date 2017 1 1) "Paycheck"])]
      (is (thrown? IllegalStateException
                   (models/delete transaction)))
      (is (models/find transaction)
          "The transaction can be retrieved after failed delete attempt"))))
