(ns clj-money.entities.transactions-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.db.ref]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.test_assertions]
            [clj-money.config :refer [env]]
            [clj-money.util :as util]
            [clj-money.entity-helpers :as helpers :refer [assert-deleted]]
            [clj-money.db :as db]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.entities.ref]
            [clj-money.entities.transactions :as transactions]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-transaction]]
            [clj-money.test-helpers :refer [dbtest]]
            [clj-money.spies :as spy]))

(def ^:private reload-account
  (comp entities/find
        find-account))

(defn- assert-account-quantities
  [& {:as balances}]
  (doseq [[account balance] balances]
    (is (= balance (:account/quantity (entities/find account)))
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
                :items [#:transaction-item{:debit-item {:account-item/account (find-account "Checking")
                                                        :account-item/action :debit
                                                        :account-item/quantity 1000M}
                                           :credit-item {:account-item/account (find-account "Salary")
                                                         :account-item/action :credit
                                                         :account-item/quantity 1000M
                                                         :account-item/memo "conf # 123"}
                                           :value 1000M}]})

(defn- assert-created
  [attr]
  (helpers/assert-created attr
                          :refs [:transaction/entity
                                 :transaction-item/account
                                 :account-item/account]
                          :compare-result? false
                          :ignore-nils? true))

(defn- assert-invalid
  [attr errors]
  (helpers/assert-invalid attr errors :put-opts {:spec :bilateral}))

(dbtest create-a-transaction
  (with-context base-context
    (assert-created (attributes))))

(dbtest ^:multi-threaded create-and-propagate-a-transaction
        (with-context base-context
          (let [date (t/local-date 2016 3 2)]
            (prop/put-and-propagate (attributes))
            (testing "entity updates"
                (is (comparable? #:entity{:transaction-date-range [date date]}
                                 (entities/find (find-entity "Personal")))
                    "The entity is updated with the transaction dates"))
            (testing "account updates"
                (is (comparable? #:account{:transaction-date-range [date date]}
                                 (reload-account "Checking"))
                    "The debited account is updated with transaction dates")
                (is (comparable? #:account{:transaction-date-range [date date]}
                                 (reload-account "Salary"))
                    "The credited account is updated with transaction dates"))
            (testing "item updates"
              (is (seq-of-maps-like? [#:account-item{:index 0
                                                     :balance 1000M}
                                      #:account-item{:index 0
                                                     :balance 1000M}]
                                     (entities/select
                                       (util/entity-type
                                         {:transaction/transaction-date date}
                                         :account-item)))
                  "The item indices and balances are calculated")))))

(dbtest transaction-date-is-required
  (with-context base-context
    (assert-invalid (dissoc (attributes)
                            :transaction/transaction-date)
                    {:transaction/transaction-date ["Transaction date is required"]})))

(dbtest entity-is-required
  (with-context base-context
    (assert-invalid (dissoc (attributes)
                            :transaction/entity)
                    {:transaction/entity ["Entity is required"]})))

(dbtest items-must-be-specified
  (with-context base-context
    (assert-invalid (dissoc (attributes) :transaction/items)
                    {:transaction/items ["Items is required"]})))

(dbtest items-cannot-be-nil
  (with-context base-context
    (assert-invalid (assoc (attributes) :transaction/items nil)
                    {:transaction/items ["Items must be a list of values"]})))

(dbtest items-cannot-be-empty
  (with-context base-context
    (assert-invalid (assoc (attributes) :transaction/items [])
                    {:transaction/items ["Items must contain at least 1 item(s)"]})))

(dbtest item-debit-item-is-required
  (with-context base-context
    (try (entities/put (update-in
                         (attributes)
                         [:transaction/items 0]
                         dissoc
                         :transaction-item/debit-item))
         (is false "Expected an exception, but none was thrown")
         (catch clojure.lang.ExceptionInfo e
           (is (= ["Debit item is required"]
                  (get-in (ex-data e)
                       [:dgknght.app-lib.validation/errors
                        :transaction/items
                        0
                        :transaction-item/debit-item])))))))

(dbtest item-credit-item-is-required
  (with-context base-context
    (try (entities/put (update-in
                         (attributes)
                         [:transaction/items 0]
                         dissoc
                         :transaction-item/credit-item))
         (is false "Expected an exception, but none was thrown")
         (catch clojure.lang.ExceptionInfo e
           (is (= ["Credit item is required"]
                  (get-in (ex-data e)
                       [:dgknght.app-lib.validation/errors
                        :transaction/items
                        0
                        :transaction-item/credit-item])))))))

(dbtest item-value-is-required
  (with-context base-context
    (assert-invalid (update-in (attributes)
                               [:transaction/items 0]
                               #(-> %
                                    (dissoc :transaction-item/value)
                                    (assoc :transaction-item/quantity 1M)))
                    {:transaction/items
                     {0
                      {:transaction-item/value ["Value is required"]}}})))

(dbtest item-value-must-be-greater-than-zero
  (with-context base-context
    (assert-invalid (assoc-in
                          (attributes)
                          [:transaction/items
                           0
                           :transaction-item/value]
                          -1000M)
                    {:transaction/items
                     {0
                      {:transaction-item/value ["Value must be a positive number"]}}})))

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
                      :quantity 100M}))

(dbtest ^:multi-threaded insert-transaction-before-the-end
  (with-context insert-context
    (prop/put-and-propagate
      #:transaction{:transaction-date (t/local-date 2016 3 3)
                    :entity (find-entity "Personal")
                    :description "Kroger"
                    :debit-account (find-account "Groceries")
                    :credit-account (find-account "Checking")
                    :quantity 99M})
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
                  :commodity "USD"}))

(dbtest ^:multi-threaded create-a-transaction-with-multiple-items-for-one-account
  (with-context multi-context
    (prop/put-and-propagate
      #:transaction{:transaction-date (t/local-date 2016 3 2)
                    :entity (find-entity "Personal")
                    :description "Paycheck"
                    :items [#:transaction-item{:action :debit
                                               :account (find-account "Checking")
                                               :quantity 1000M}
                            #:transaction-item{:action :debit
                                               :account (find-account "Checking")
                                               :quantity 100M}
                            #:transaction-item{:action :credit
                                               :account (find-account "Salary")
                                               :quantity 1000M}
                            #:transaction-item{:action :credit
                                               :account (find-account "Bonus")
                                               :quantity 100M}]})
    (is (comparable? {:account/transaction-date-range [(t/local-date 2016 3 2)
                                                       (t/local-date 2016 3 2)]}
                     (reload-account "Checking"))
        "The checking account transaction date boundaries reflect all transactions")
    (is (seq-of-maps-like? [{:transaction-item/index 1
                             :transaction-item/action :debit
                             :transaction-item/quantity 100M
                             :transaction-item/balance 1100M}
                            {:transaction-item/index 0
                             :transaction-item/action :debit
                             :transaction-item/quantity 1000M
                             :transaction-item/balance 1000M}]
                           (items-by-account "Checking"))
        "The checking account items has sequential indices and a running balance")))

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

(dbtest delete-a-transaction
  (with-context delete-context
    (assert-deleted (find-transaction [(t/local-date 2016 3 3) "Kroger"]))))

(dbtest ^:multi-threaded delete-and-propagate-a-transaction
  (with-context delete-context
    (let [checking-items-before (items-by-account "Checking")
          trans (find-transaction [(t/local-date 2016 3 3) "Kroger"])]
      (prop/delete-and-propagate trans)
      (testing "checking transaction item balances are adjusted"
        (is (seq-of-maps-like? [#:transaction-item{:index 2 :quantity 102M :balance 798M}
                                #:transaction-item{:index 1 :quantity 100M :balance 900M}
                                #:transaction-item{:index 0 :quantity 1000M :balance 1000M}]
                               checking-items-before)
            "The item to be deleted is present before the delete")
        (is (seq-of-maps-like? [#:transaction-item{:index 1 :quantity 102M :balance 898M}
                                #:transaction-item{:index 0 :quantity 1000M :balance 1000M}]
                               (items-by-account "Checking"))
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

(dbtest get-a-transaction
  (with-context update-context
    (let [retrieved (entities/find-by
                      {:transaction/transaction-date (t/local-date 2016 3 2)
                       :transaction/description "Paycheck"})]
      (is (comparable? {:transaction/value 1000M
                        :transaction/description "Paycheck"
                        :transaction/transaction-date (t/local-date 2016 3 2)}
                       retrieved)
          "The transaction can be retrieved")
      (is (= 2 (count (:transaction/items retrieved)))
          "The transaction items are included"))))

(def search-context
  (conj base-context
        #:transaction{:transaction-date #clj-money/local-date "2016-01-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 160101M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #clj-money/local-date "2016-06-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 160601M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #clj-money/local-date "2017-01-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 170101M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #clj-money/local-date "2017-06-01"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 170601M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:transaction-date #clj-money/local-date "2017-06-15"
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 170615M
                      :debit-account "Checking"
                      :credit-account "Salary"}))

(dbtest search-by-date
  (with-context search-context
    (is (seq-of-maps-like? [#:transaction{:transaction-date (t/local-date 2017 6 15)
                                          :description "Paycheck"
                                          :value 170615M}]
                           (entities/select #:transaction{:transaction-date (t/local-date 2017 6 15)
                                                        :entity (find-entity "Personal")}))
        "The transactions from the specified day are returned")))

(dbtest search-by-date-vector
  (with-context search-context
    (is (seq-of-maps-like?
          [#:transaction{:transaction-date (t/local-date 2017 6 1)}
           #:transaction{:transaction-date (t/local-date 2017 6 15)}]
          (entities/select #:transaction{:transaction-date [:between
                                                          (t/local-date 2017 6 1)
                                                          (t/local-date 2017 6 30)]
                                       :entity (find-entity "Personal")}))
        "The transactions from the specified date range are returned")))

(defn- update-items
  [items change-map]
  (let [indexed-items (index-by (comp util/->entity-ref
                                      :transaction-item/account)
                                items)]
    (->> change-map
         (map #(update-in % [0] util/->entity-ref))
         (reduce (fn [items [account item]]
                   (update-in items [account] merge item))
                 indexed-items)
         vals
         (into []))))

(defn- update-trx-items
  [trx & {:as change-map}]
  (prop/put-and-propagate
    (update-in trx [:transaction/items] update-items change-map)))

(dbtest ^:multi-threaded update-a-transaction-change-quantity
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")]
      (update-trx-items (find-transaction [(t/local-date 2016 3 12) "Kroger"])
                        groceries {:transaction-item/quantity 99.99M}
                        checking {:transaction-item/quantity 99.99M})
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

(dbtest ^:multi-threaded update-a-transaction-change-date
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          trx (find-transaction [(t/local-date 2016 3 22) "Kroger"])
          result (-> trx
                       (assoc :transaction/transaction-date (t/local-date 2016 3 10))
                       prop/put-and-propagate)]
      (is (seq-of-maps-like? [{:transaction-item/index 2
                               :transaction/transaction-date (t/local-date 2016 3 12)
                               :transaction-item/quantity 101M
                               :transaction-item/balance 797M}
                              {:transaction-item/index 1
                               :transaction/transaction-date (t/local-date 2016 3 10)
                               :transaction-item/quantity 102M
                               :transaction-item/balance 898M}
                              {:transaction-item/index 0
                               :transaction/transaction-date (t/local-date 2016 3 2)
                               :transaction-item/quantity 1000M
                               :transaction-item/balance 1000M}]
                             (items-by-account checking))
          "The checking account items are updated")
      (is (seq-of-maps-like? [{:transaction-item/index 1
                               :transaction/transaction-date (t/local-date 2016 3 12)
                               :transaction-item/quantity 101M
                               :transaction-item/balance 203M}
                              {:transaction-item/index 0
                               :transaction/transaction-date (t/local-date 2016 3 10)
                               :transaction-item/quantity 102M
                               :transaction-item/balance 102M}]
                             (items-by-account groceries))
          "The groceries account items are updated")
      (assert-account-quantities checking 797M groceries 203M)
      (testing "transaction is updated"
        (is (= (t/local-date 2016 3 10)
               (:transaction/transaction-date (entities/find result)))
            "The updated transaction can be retrieved")))))

(dbtest ^:multi-threaded update-a-transaction-cross-partition-boundary
  (with-context update-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")
          result (-> (find-transaction [(t/local-date 2016 3 12) "Kroger"])
                     (assoc :transaction/transaction-date (t/local-date 2016 4 12))
                     prop/put-and-propagate)]
      (is (seq-of-maps-like? [{:transaction-item/index 2
                               :transaction-item/quantity 101M
                               :transaction-item/balance 797M
                               :transaction/transaction-date (t/local-date 2016 4 12)}
                              {:transaction-item/index 1
                               :transaction-item/quantity 102M
                               :transaction-item/balance 898M
                               :transaction/transaction-date (t/local-date 2016 3 22)}
                              {:transaction-item/index 0
                               :transaction-item/quantity 1000M
                               :transaction-item/balance 1000M
                               :transaction/transaction-date (t/local-date 2016 3 2)}]
                             (items-by-account checking))
          "The checking account items reflect the change in transaction date")
      (is (seq-of-maps-like? [{:transaction-item/index 1
                               :transaction-item/quantity 101M
                               :transaction-item/balance 203M
                               :transaction/transaction-date (t/local-date 2016 4 12) }
                              {:transaction-item/index 0
                               :transaction-item/quantity 102M
                               :transaction-item/balance 102M
                               :transaction/transaction-date (t/local-date 2016 3 22)}]
                             (items-by-account groceries))
          "The groceries account items reflect the change in transaction date")
      (assert-account-quantities checking 797M groceries 203M)
      (testing "transaction is updated"
        (is (= (t/local-date 2016 4 12)
               (:transaction/transaction-date (entities/find result)))
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

(deftest update-a-transaction-short-circuit-propagation
  (let [storage (-> env
                    (get-in [:db :strategies :datomic-peer])
                    (db/reify-storage)
                    db/reset
                    spy/storage-spy)]
    (db/with-storage [storage]
      (with-context short-circuit-context
        (spy/clear storage)
        (-> (find-transaction [(t/local-date 2016 3 16)
                               "Kroger"])
            (assoc :transaction/transaction-date
                   (t/local-date (t/local-date 2016 3 8)))
            prop/put-and-propagate)
        (let [[[c1] [c2] :as cs] (spy/calls storage :put)
              checking (find-account "Checking")]
          (is (= 2 (count cs))
              "Two calls are made to write to storage (the primary and the propagation)")
          (is (seq-of-maps-like?
                [#:transaction{:transaction-date (t/local-date 2016 3 8)
                               :description "Kroger"}]
                c1)
              "The first call puts the modified transaction")
          (is (seq-of-maps-like?
                [#:transaction-item{:index 1
                                    :quantity 102M
                                    :balance 898M}
                 #:transaction-item{:index 2
                                    :quantity 101M
                                    :balance 797M}]
                (filter (every-pred
                          (util/entity-type? :transaction-item)
                          (comp (partial util/entity= checking)
                                :transaction-item/account))
                        c2))
              "The second call includes the updated line items")
          (is (->> c2
                   (filter (every-pred
                             (util/entity-type? :transaction-item)
                             (comp (partial util/entity= checking)
                                   :transaction-item/account)
                             (comp #{3 4}
                                   :transaction-item/index)))
                   empty?)
              "The unchanged items are not updated")
          (is (->> c2
                   (filter (util/entity-type? :account))
                   empty?)
              "The account is not updated"))))))

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

(dbtest ^:multi-threaded update-a-transaction-change-account
  (with-context change-account-context
    (let [[rent
           groceries] (find-accounts "Rent" "Groceries")]
      (update-trx-items (find-transaction [(t/local-date 2016 3 16) "Kroger"])
                        groceries {:transaction-item/account rent})
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

(dbtest ^:multi-threaded update-a-transaction-change-action
  (with-context change-action-context
    (let [checking (find-account "Checking")
          groceries (find-account "Groceries")]
      (update-trx-items (find-transaction [(t/local-date 2016 3 16) "Kroger"])
                        groceries {:transaction-item/action :credit}
                        checking {:transaction-item/action :debit})
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
                      :quantity 103M}
        #:transaction{:transaction-date (t/local-date 2016 3 16)
                      :entity "Personal"
                      :description "Kroger"
                      :items [#:transaction-item{:value 90M
                                                 :debit-item #:account-item{:action :debit
                                                                            :account "Groceries"
                                                                            :quantity 90M}
                                                 :credit-item #:account-item{:action :credit
                                                                             :account "Checking"
                                                                             :quantity -90M}}
                              #:transaction-item{:value 12M
                                                 :debit-item #:account-item{:action :debit
                                                                            :account "Pets"
                                                                            :quantity 12M}
                                                 :credit-item #:account-item{:action :credit
                                                                             :account "Checking"
                                                                             :quantity -12M}}]}
        #:transaction{:transaction-date (t/local-date 2016 3 23)
                      :entity "Personal"
                      :description "Kroger"
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :quantity 101M}))

(dbtest update-a-transaction-remove-item
  (with-context add-remove-item-context
    (-> (find-transaction [(t/local-date 2016 3 16) "Kroger"])
        (update-in [:transaction/items]
                   #(take 1 %))
        prop/put-and-propagate)
    (let [checking (reload-account "Checking")
          pets (reload-account "Pets")]
      (is (= (- 1000M 103M 90M 101M)
             (:account/value checking))
          "The credit account balance is adjusted")
      (is (= (- 0M)
             (:account/value pets))
          "The debit account balance is adjusted")
      (is (seq-of-maps-like?
            [{:account-item/index 0
              :account-item/quantity 1000M
              :account-item/balance 1000M}
             {:account-item/index 1
              :account-item/quantity -103M
              :account-item/balance 897M}
             {:account-item/index 2
              :account-item/quantity -90M
              :account-item/balance 807M}
             {:account-item/index 3
              :account-item/quantity -101M
              :account-item/balance 706M}]
            (entities/select {:account-item/account checking}
                             {:sort [:account-item/index]}))
          "The credit account items reflect the removal and are re-indexed")
      (is (empty? (entities/select {:account-item/account pets}))
          "The debit account items reflect the removal"))))

(dbtest update-a-transaction-add-item
  (with-context add-remove-item-context
    (let [[pets
           groceries
           checking] (find-accounts "Pets" "Groceries" "Checking")]
      (-> (find-transaction [(t/local-date 2016 3 9) "Kroger"])
          (update-in [:transaction/items]
                     update-items
                     {groceries #:transaction-item{:quantity 90M
                                                   :value 90M}})
          (update-in [:transaction/items]
                     conj
                     #:transaction-item{:action :debit
                                        :account pets
                                        :quantity 13M
                                        :value 13M})
          prop/put-and-propagate)
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

(dbtest get-a-balance-delta
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

(dbtest get-a-balance-as-of
  (with-context balance-delta-context
    (let [checking (reload-account "Checking")]
      (is (= 2001M
             (transactions/balance-as-of checking 2016 1 31))
          "The January value is the balance for the last item in the period")
      (is (= 4203M
             (transactions/balance-as-of checking 2016 2 29))
          "The February value is the balance for the last item in the period"))))

(dbtest create-multiple-transactions-then-recalculate-balances
  (with-context base-context
    (let [entity (find-entity "Personal")
          [checking
           salary
           groceries] (find-accounts "Checking" "Salary" "Groceries")]
      (transactions/with-delayed-propagation [out-chan ctrl-chan]
        (mapv (comp #(entities/put %
                                 :out-chan out-chan
                                 :close-chan? false
                                 :ctrl-chan ctrl-chan)
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
                             :quantity 1000M}]))
      (is (= 1900M (:account/quantity (reload-account "Checking")))
          "The account balance is recalculated after the form exits")
      (is (comparable? {:entity/transaction-date-range [(t/local-date 2017 1 1)
                                                        (t/local-date 2017 2 1)]}
                       (entities/find entity))
          "The entity transaction date boundaries are updated"))))

(dbtest use-simplified-items
  (with-context base-context
    (let [entity (find-entity "Personal")
          [checking salary] (find-accounts "Checking" "Salary")
          trx (entities/put #:transaction{:entity entity
                                          :transaction-date (t/local-date 2017 3 2)
                                          :description "Paycheck"
                                          :quantity 1000M
                                          :debit-account checking
                                          :credit-account salary})]
      (is (seq-of-maps-like? [#:transaction-item{:value 1000M
                                                 :debit-item {:account-item/account (util/->entity-ref checking)
                                                              :account-item/action :debit}
                                                 :credit-item {:account-item/account (util/->entity-ref salary)
                                                               :account-item/action :credit}}]
                             (:transaction/items trx))))))

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
                         :items [[(t/local-date 2017 1 1)
                                  1000M]]}))

(dbtest the-quantity-of-a-reconciled-item-cannot-be-changed
  (with-context existing-reconciliation-context
    (-> (find-transaction [(t/local-date 2017 1 1) "Paycheck"])
        (assoc-in [:transaction/items 0 :transaction-item/quantity] 1010M)
        (assoc-in [:transaction/items 1 :transaction-item/quantity] 1010M)
        (assert-invalid {:transaction/items ["A reconciled quantity cannot be updated"]}))))

(dbtest the-action-of-a-reconciled-item-cannot-be-changed
  (with-context existing-reconciliation-context
    (-> (find-transaction [(t/local-date 2017 1 1) "Paycheck"])
        (assoc-in [:transaction/items 0 :transaction-item/action] :credit)
        (assoc-in [:transaction/items 1 :transaction-item/action] :debit)
        (assert-invalid {:transaction/items ["A reconciled quantity cannot be updated"]}))))

(dbtest a-reconciled-transaction-item-cannot-be-deleted
  (with-context existing-reconciliation-context
    (let [transaction (find-transaction [(t/local-date 2017 1 1) "Paycheck"])]
      (is (thrown? IllegalStateException
                   (entities/delete transaction)))
      (is (entities/find transaction)
          "The transaction can be retrieved after failed delete attempt"))))

(def ^:private migrate-context
  (conj base-context
        #:account{:name "Savings"
                  :entity "Personal"
                  :type :asset}
        #:transaction{:entity "Personal"
                      :transaction-date (t/local-date 2015 1 1)
                      :description "Paycheck"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        #:transaction{:entity "Personal"
                      :transaction-date (t/local-date 2015 1 2)
                      :description "Kroger"
                      :quantity 100M
                      :debit-account "Groceries"
                      :credit-account "Checking"}))

(dbtest migrate-items-from-one-account-to-another
  (with-context migrate-context
    (let [checking (find-account "Checking")
          savings (find-account "Savings")]
      (transactions/migrate-account checking savings)
      (is (comparable? #:account{:quantity 0M}
                       (entities/find checking))
          "The from account has a zero balance after the transfer")
      (is (comparable? #:account{:quantity 900M}
                       (entities/find savings))
          "The to account has the balance the from account had before the transfer"))))
