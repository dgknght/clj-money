(ns clj-money.models.budgets-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.db.sql.ref]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.util :refer [model=]]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-deleted]]
            [clj-money.models.budgets :as budgets]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-account
                                            find-budget]]
            [clj-money.test-helpers :refer [reset-db
                                            account-ref]]))

(use-fixtures :each reset-db)

(defn- attributes []
  #:budget{:entity (find-entity "Personal")
           :name "2016"
           :start-date (t/local-date 2016 1 1)
           :period [3 :month]
           :items [#:budget-item{:account (find-account "Salary")
                                 :periods [1000M 1001M 1002M]
                                 :spec nil}
                   #:budget-item{:account (find-account "Rent")
                                 :periods [500M 500M 500M]
                                 :spec ^:no-prune {:average 500M}}
                   #:budget-item{:account (find-account "Groceries")
                                 :periods [100M 90M 105M]
                                 :spec nil}]})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:budget/entity :budget-item/account]))

(deftest create-a-budget
  (with-context basic-context
    (let [created (assert-created (attributes))]
      (is (every? :id (:budget/items created))
          "The items have id values")
      (is (t/= (t/local-date 2016 3 31)
               (:budget/end-date created))
          "The end date is calculated and included in the result"))))

(deftest entity-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :budget/entity)
                    {:budget/entity ["Entity is required"]})))

(deftest name-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :budget/name)
                    {:budget/name ["Name is required"]})))

(deftest start-date-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :budget/start-date)
                    {:budget/start-date ["Start date is required"]})))

(deftest period-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :budget/period)
                    {:budget/period ["Period is required"]})))

(deftest period-type-is-required
  (with-context
    (assert-invalid (assoc (attributes) :budget/period [12])
                    {:budget/period ["Period is invalid"]}
                    :message "An omitted period type is invalid")
    (assert-invalid (assoc (attributes) :budget/period [12 nil])
                    {:budget/period {1 ["Value must be quarter, week, or month"]}}
                    :message "A nil period type is invalid")))

(deftest period-type-must-be-week-month-or-quarter
  (with-context
    (assert-invalid (assoc (attributes) :budget/period [12 :not-a-period])
                    {:budget/period {1 ["Value must be quarter, week, or month"]}})))

(deftest period-count-is-required
  (with-context
    (assert-invalid (assoc (attributes) :budget/period [nil :month])
                    {:budget/period {0 ["Value must be an integer"]}})))

(deftest period-count-must-be-greater-than-zero
  (with-context
    (assert-invalid (assoc (attributes) :budget/period [0 :month])
                    {:budget/period {0 ["Value is invalid"]}})))

(def existing-context
  (conj basic-context
        #:budget{:name "2016"
                 :entity "Personal"
                 :period [12 :month]
                 :start-date (t/local-date 2016 1 1)
                 :items [#:budget-item{:account "Salary"
                                       :periods (repeat 12 1000M)}
                         #:budget-item{:account "Rent"
                                       :periods (repeat 12 500M)}
                         #:budget-item{:account "Groceries"
                                       :periods (repeat 12 100M)}]}))

(deftest delete-a-budget
  (with-context existing-context
    (assert-deleted (find-budget "2016"))))

(defn- find-item-by-account
  [{:budget/keys [items]} {:keys [id]}]
  (->> items
       (filter #(= id (get-in % [:budget-item/account :id])))
       first))

(deftest update-a-budget
  (with-context existing-context
    (let [budget (find-budget "2016")
          salary (find-account "Salary")
          result (models/put
                   (-> budget
                       (assoc :budget/name "edited")
                       (assoc :budget/start-date (t/local-date 2015 1 1))
                       (assoc-in [:budget/items 0 :budget-item/periods] (vec (repeat 12 1100M)))))
          expected #:budget{:name "edited"
                            :start-date (t/local-date 2015 1 1)
                            :end-date (t/local-date 2015 12 31)}
          retrieved (models/find budget)]
      (is (comparable? expected result)
          "The return value has the updated attributes")
      (is (= (repeat 12 1100M)
             (:budget-item/periods (find-item-by-account result salary)))
          "The returned value reflects the updated items")
      (is (comparable? expected retrieved)
          "The retrieved value has the updated attributes")
      (is (= (repeat 12 1100M)
             (:budget-item/periods (find-item-by-account retrieved salary)))
          "The retrieved value has the updated items"))))

(deftest add-an-item
  (with-context existing-context
    (let [budget (find-budget "2016")
          fit (find-account "FIT")
          result (models/put
                   (update-in budget
                              [:budget/items]
                              conj
                              #:budget-item{:account fit
                                            :periods (vec (repeat 12 100M))}))]
      (is (= 4 (count (:budget/items result)))
          "The return value has the new item")
      (is (= 4 (count (:budget/items (models/find budget))))
          "The retrieved value has the new item"))))

(deftest remove-an-item
  (with-context existing-context
    (let [budget (find-budget "2016")
          groceries (account-ref "Groceries")]
      (-> budget
          (update-in [:budget/items]
                     (fn [items]
                       (remove #(model= groceries
                                   (:budget-item/account %))
                               items)))
          budgets/update-items)
      (is (= 2 (count (:budget/items (models/find budget))))
          "The retrieved value does not have the removed item"))))

(deftest budget-item-requires-an-account
  (with-context
    (assert-invalid (update-in (attributes)
                               [:budget/items 0]
                               dissoc
                               :budget-item/account)
                    {:budget/items {0 {:budget-item/account ["Account is required"]}}})))

(deftest budget-item-account-must-belong-to-budget-entity
  (with-context
    (assert-invalid (assoc-in (attributes)
                              [:budget/items 0 :budget-item/account]
                              (find-account "Sales")) ; Sales is in the Business entity
                    {:budget/items ["All accounts must belong to the budget entity"]})))

(deftest budget-item-has-same-period-count-as-budget
  (with-context
    (assert-invalid
      (assoc-in (attributes)
                [:budget/items 0 :budget-item/periods]
                [100M])
      {:budget/items
       ["All items must have a number of periods that matches the budget period count"]})))

(deftest find-a-budget-by-date
  (with-context existing-context
    (let [entity (find-entity "Personal")
          tests [{:description "before any budgets"
                  :date (t/local-date 2015 12 31)
                  :expected nil}
                 {:description "start of a budget"
                  :date (t/local-date 2016 1 1)
                  :expected "2016"}]]
      (doseq [{:keys [expected date description]} tests]
        (testing description
          (is (= expected
                 (:budget/name (budgets/find-by-date entity date)))))))))

(def ^:private get-items-context
  (conj basic-context
        #:account{:name "Food"
                  :parent "Groceries"
                  :entity "Personal"
                  :type :expense}
        #:account{:name "Non-food"
                  :parent "Groceries"
                  :entity "Personal"
                  :type :expense}
        #:budget{:entity "Personal"
                 :name "2015"
                 :start-date (t/local-date 2015 1 1)
                 :period [3 :month]
                 :items [#:budget-item{:account "Food"
                                       :periods (repeat 3 100M)}
                         #:budget-item{:account "Non-food"
                                       :periods (repeat 3 50M)}]}))

(deftest get-items-by-account
  (with-context get-items-context
    (let [budget (find-budget "2015")]
      (testing "a leaf account"
        (let [account (find-account "Food")
              expected [[100M 100M 100M]]
              actual (map :budget-item/periods (budgets/find-items-by-account
                                                 budget
                                                 account))]
          (is (seq-of-maps-like? expected actual) "The correct period values are returned")))
      (testing "a parent account"
        (let [account (find-account "Groceries")
              expected [[100M 100M 100M]
                        [50M 50M 50M]]
              actual (map :budget-item/periods (budgets/find-items-by-account
                                                 budget
                                                 account))]
          (is (= expected actual) "The correct period values are returned"))))))
