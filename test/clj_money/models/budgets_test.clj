(ns clj-money.models.budgets-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.db.sql.ref]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-deleted]]
            [clj-money.models.budgets :as budgets]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-account
                                            find-budget
                                            find-budget-item]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(defn- attributes []
  #:budget{:entity (find-entity "Personal")
           :name "2016"
           :start-date (t/local-date 2016 1 1)
           :period [3 :month]})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:budget/entity]))

(defn- assert-created-item
  [attr]
  (helpers/assert-created attr :refs [:budget-item/budget :budget-item/account]))

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
                    {:budget/period {1 ["Value must be quarter, day, week, month, or year"]}}
                    :message "A nil period type is invalid")))

(deftest period-type-must-be-week-month-or-quarter
  (with-context
    (assert-invalid (assoc (attributes) :budget/period [12 :not-a-period])
                    {:budget/period {1 ["Value must be quarter, day, week, month, or year"]}})))

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
                 :start-date (t/local-date 2016 1 1)}
        #:budget-item{:budget "2016"
                      :account "Salary"
                      :periods (repeat 12 1000M)}
        #:budget-item{:budget "2016"
                      :account "Rent"
                      :periods (repeat 12 500M)}
        #:budget-item{:budget "2016"
                      :account "Groceries"
                      :periods (repeat 12 100M)}))

(deftest create-a-budget-item
  (with-context existing-context
    (assert-created-item #:budget-item{:budget (find-budget "2016")
                                       :account (find-account "FIT")
                                       :periods (vec (repeat 12 100M))})))

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
          result (models/put
                   (-> budget
                       (assoc :budget/name "edited")
                       (assoc :budget/start-date (t/local-date 2015 1 1))))
          expected #:budget{:name "edited"
                            :start-date (t/local-date 2015 1 1)
                            :end-date (t/local-date 2015 12 31)}
          retrieved (models/find budget)]
      (is (comparable? expected result)
          "The return value has the updated attributes")
      (is (comparable? expected retrieved)
          "The retrieved value has the updated attributes"))))

(deftest update-an-item
  (with-context existing-context
    (let [budget (find-budget "2016")
          salary (find-account "Salary")
          item (find-item-by-account budget salary)
          result (-> item
                     (assoc :budget-item/periods (vec (repeat 12 101M)))
                     models/put)]
      (is (= #{101M} (set (:budget-item/periods result)))
          "The return value reflects the updates")
      (is (= #{101M} (set (:budget-item/periods (models/find result))))
          "The retrieved value reflects the updates"))))

(deftest remove-an-item
  (with-context existing-context
    (let [item (find-budget-item ["2016" "Groceries"])]
      (models/delete item)
      (is (nil? (models/find item))
          "The item is not retrieved after delete"))))

(deftest budget-item-requires-an-account
  (with-context existing-context
    (let [budget (find-budget "2016")]
      (assert-invalid {:budget-item/budget budget
                       :budget-item/periods (vec (repeat 12 100M))}
                      {:budget-item/account ["Account is required"]}))))

(deftest budget-item-account-must-belong-to-budget-entity
  (with-context existing-context
    (let [budget (find-budget "2016")]
      (assert-invalid {:budget-item/budget budget
                       :budget-item/account (find-account "Sales")
                       :budget-item/periods (vec (repeat 12 100M))}
                      {:budget-item/account ["Account is required"]}))))

(deftest budget-item-has-same-period-count-as-budget
  (with-context existing-context
    (let [budget (find-budget "2016")]
      (assert-invalid {:budget-item/budget budget
                       :budget-item/account (find-account "FIT")
                       :budget-item/periods (vec (repeat 11 100M))}
                      {:budget-item/periods ["All items must hav ea number of periods that matches the budget period count"]}))))

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
