(ns clj-money.entities.transactions-test
  (:require [clojure.test :refer [testing is]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.db.ref]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.test_assertions]
            [clj-money.util :as util]
            [clj-money.db.sql :as sql]
            [clj-money.entity-helpers :as helpers :refer [assert-invalid]]
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
            [clj-money.test-helpers :refer [dbtest]]))

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
                :items [#:transaction-item{:account (find-account "Checking")
                                           :action :debit
                                           :memo "conf # 123"
                                           :quantity 1000M}
                        #:transaction-item{:account (find-account "Salary")
                                           :action :credit
                                           :quantity 1000M}]})

(defn- assert-created
  [attr]
  (helpers/assert-created attr
                          :refs [:transaction/entity :transaction-item/account]
                          :compare-result? false
                          :ignore-nils? true))

(dbtest create-a-transaction
  (with-context base-context
    (assert-created (attributes))))

(dbtest create-and-propagate-a-transaction
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
        (is (seq-of-maps-like? [#:transaction-item{:index 0
                                                   :balance 1000M}
                                #:transaction-item{:index 0
                                                   :balance 1000M}]
                               (entities/select
                                 (util/entity-type
                                   {:transaction/transaction-date date}
                                   :transaction-item)))
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

(dbtest items-are-required
  (with-context base-context
    (assert-invalid (assoc (attributes) :transaction/items [])
                    {:transaction/items ["Items must contain at least 1 item(s)"]})))

(dbtest item-account-is-required
  (with-context base-context
    (assert-invalid (update-in
                      (attributes)
                      [:transaction/items 0]
                      dissoc
                      :transaction-item/account)
                    {:transaction/items
                     {0
                      {:transaction-item/account ["Account is required"]}}})))

(dbtest item-quantity-is-required
  (with-context base-context
    (assert-invalid (update-in (attributes)
                               [:transaction/items 0]
                               #(-> %
                                    (dissoc :transaction-item/quantity)
                                    (assoc :transaction-item/value 1M)))
                    {:transaction/items
                     {0
                      {:transaction-item/quantity ["Quantity is required"]}}})))

(dbtest item-quantity-must-be-greater-than-zero
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

(dbtest item-action-is-required
  (with-context base-context
    (assert-invalid (update-in
                          (attributes)
                          [:transaction/items 0]
                          #(dissoc % :transaction-item/action))
                    {:transaction/items
                     {0
                      {:transaction-item/action ["Action is required"]}}})))

(dbtest item-action-must-be-debit-or-credit
  (with-context base-context
    (assert-invalid (assoc-in
                      (attributes)
                      [:transaction/items
                       0
                       :transaction-item/action]
                      :not-valid)
                    {:transaction/items
                     {0
                      {:transaction-item/action ["Action is invalid"]}}})))

(dbtest sum-of-debits-must-equal-sum-of-credits
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

(dbtest insert-transaction-before-the-end
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

(dbtest create-a-transaction-with-multiple-items-for-one-account
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

(dbtest update-a-transaction-change-quantity
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

(dbtest update-a-transaction-change-date
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

(dbtest update-a-transaction-cross-partition-boundary
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

; TODO: Consider mocking Storage instead of put*
(dbtest update-a-transaction-short-circuit-updates {:only :sql}
  (with-context short-circuit-context
    (let [calls (atom [])
          orig-put sql/put*]
      (with-redefs [sql/put* (fn [ds entities]
                               (swap! calls conj entities)
                               (orig-put ds entities))]
        (-> (find-transaction [(t/local-date 2016 3 16) "Kroger"])
            (assoc :transaction/transaction-date (t/local-date 2016 3 8))
            prop/put-and-propagate)
        (let [[c1 c2 :as cs] @calls
              checking (find-account "Checking")]
          (is (= 2 (count cs))
              "Two calls are made to write to storage (the primary and the propagation)")
          (is (seq-of-maps-like? [#:transaction{:description "Kroger"
                                                :transaction-date (t/local-date 2016 3 8)}]
                                 (filter (util/entity-type? :transaction)
                                         c1))
              "The updated transaction is written in the 1st call")
          (is (seq-of-maps-like? [#:transaction-item {:index 1
                                                      :quantity 102M
                                                      :balance 898M}
                                  #:transaction-item{:index 2
                                                     :quantity 101M
                                                     :balance 797M}]
                                 (filterv (every-pred (util/entity-type? :transaction-item)
                                                      #(util/entity= (:transaction-item/account %)
                                                                    checking))
                                          c2))
              "The affected transaction items are written in the 2nd call")

          (is (empty? (filter #(#{3 4} (:transaction-item/index %))
                              (flatten cs)))
              "The unaffected transaction items are not written")
          (is (empty? (filter (util/entity-type? :account)
                              (flatten cs)))
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

(dbtest update-a-transaction-change-account
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

(dbtest update-a-transaction-change-action
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

(dbtest update-a-transaction-remove-item
  (with-context add-remove-item-context
    (-> (find-transaction [(t/local-date 2016 3 16) "Kroger"])
        (update-in [:transaction/items]
                   update-items
                   {(find-account "Groceries")
                    #:transaction-item{:quantity 102M
                                       :value 102M}})
        (update-in [:transaction/items] #(remove (fn [i]
                                                   (= 12M (:transaction-item/quantity i)))
                                                 %))
        prop/put-and-propagate)))

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
             (transactions/balance-as-of checking
                                         (t/local-date 2016 1 31)))
          "The January value is the balance for the last item in the period")
      (is (= 4203M
             (transactions/balance-as-of checking
                                         (t/local-date 2016 2 29)))
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
      (is (seq-of-maps-like? [#:transaction-item{:quantity 1000M
                                                 :action :debit
                                                 :account (util/->entity-ref checking)}
                              #:transaction-item{:quantity 1000M
                                                 :action :credit
                                                 :account (util/->entity-ref salary)}]
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
