(ns clj-money.models.budgets-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.db.sql.ref]
            [clj-money.models :as models]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-deleted]]
            [clj-money.models.budgets :as budgets]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-account
                                            find-budget]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(defn- attributes []
  #:budget{:entity (find-entity "Personal")
           :name "2016"
           :start-date (t/local-date 2016 1 1)
           :period :month
           :period-count 3
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

(deftest period-must-be-week-month-or-quarter
  (with-context
    (assert-invalid (assoc (attributes) :budget/period :not-a-period)
                    {:budget/period ["Period must be week or month"]})))

(deftest period-count-is-required
  (with-context
    (assert-invalid (dissoc (attributes) :budget/period-count)
                    {:budget/period-count ["Period count is required"]})))

(deftest period-count-must-be-greater-than-zero
  (with-context
    (assert-invalid (assoc (attributes) :budget/period-count 0)
                    {:budget/period-count ["Period count must be greater than zero"]})))

(def existing-context
  (conj basic-context
        #:budget{:name "2016"
                 :entity "Personal"
                 :period :month
                 :period-count 12
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
                       (assoc-in [:budget/items 0 :budget-item/periods] (repeat 12 1100M))))
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

; ;; Periods
; 
; ; TODO: Move these to cljc?
; 
; (deftest get-a-budget-end-date
;   (testing "a monthly budget"
;     (is (= (t/local-date 2017 12 31)
;            (budgets/end-date {:period :month
;                               :period-count 12
;                               :start-date (t/local-date 2017 1 1)}))
;         "It returns the last date of the last month"))
;   (testing "a weekly budget"
;     (is (= (t/local-date 2017 2 4)
;            (budgets/end-date {:period :week
;                               :period-count 5
;                               :start-date (t/local-date 2017 1 1)}))
;         "It returns the last date of the last week")))
; 
; (deftest get-the-index-of-the-period-containing-a-date
;   (let [all-tests [{:period :month
;                     :tests [{:date (t/local-date 2017 1 1)
;                              :expected 0}
;                             {:date (t/local-date 2017 1 31)
;                              :expected 0}
;                             {:date (t/local-date 2017 2 1)
;                              :expected 1}
;                             {:date (t/local-date 2017 2 28)
;                              :expected 1}
;                             {:date (t/local-date 2017 3 1)
;                              :expected 2}
;                             {:date (t/local-date 2017 12 31)
;                              :expected 11}
;                             {:date (t/local-date 2016 12 31)
;                              :expected nil}
;                             {:date (t/local-date 2018 1 1)
;                              :expected nil}]}
;                    {:period :week
;                     :tests [{:date (t/local-date 2017 1 1)
;                              :expected 0}
;                             {:date (t/local-date 2017 1 7)
;                              :expected 0}
;                             {:date (t/local-date 2017 1 8)
;                              :expected 1}
;                             {:date (t/local-date 2017 3 25)
;                              :expected 11}
;                             {:date (t/local-date 2016 12 31)
;                              :expected nil}
;                             {:date (t/local-date 2017 3 26)
;                              :expected nil}]}]
;         budget {:start-date (t/local-date 2017 1 1)
;                 :period-count 12}]
;     (doseq [{:keys [period tests]} all-tests]
;       (testing (format "period %s" period)
;         (doseq [{:keys [date expected]} tests]
;           (is (= expected (:index (budgets/period-containing
;                                    (assoc budget :period period)
;                                    date)))
;               (format "Given a budget starting on 1/1/2017, %s produces %s" date expected)))))
; 
;     (testing "monthly budget"
;       (is (= 3 (:index (budgets/period-containing
;                         (assoc budget :period :month)
;                         (t/local-date 2017 4 3))))
;           "It returns the index of the period containing the date"))
;     (testing "weekly budget"
;       (is (= 3 (:index (budgets/period-containing
;                         (assoc budget :period :week)
;                         (t/local-date 2017 1 25))))
;           "It returns the index of the period containing the date"))))
; 
; (deftest find-a-budget-by-date
;   (let [context (realize delete-context)
;         entity-id (-> context :entities first :id)
;         tests [{:description "before any budgets"
;                 :date (t/local-date 2015 12 31)
;                 :expected nil}
;                {:description "start of a budget"
;                 :date (t/local-date 2016 1 1)
;                 :expected "2016"}]]
;     (doseq [{:keys [expected date description]} tests]
;       (testing description
;         (is (= expected
;                (:name (budgets/find-by-date entity-id date))))))))
; 
; (deftest calculate-a-percent-of-a-period
;   (let [tests [{:description "the first day of a month"
;                 :budget {:period :month
;                          :period-count 12
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 1 1)
;                 :expected 1/31}
;                {:description "the 15th day of a month"
;                 :budget {:period :month
;                          :period-count 12
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 4 15)
;                 :expected 1/2}
;                {:description "the last day of a month"
;                 :budget {:period :month
;                          :period-count 12
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 1 31)
;                 :expected 1/1}
;                {:description "the first day of a week"
;                 :budget {:period :week
;                          :period-count 8
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 1 1)
;                 :expected 1/7}
;                {:description "the 4th day of a week"
;                 :budget {:period :week
;                          :period-count 8
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 1 4)
;                 :expected 4/7}
;                {:description "the last day of a week"
;                 :budget {:period :week
;                          :period-count 8
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 1 7)
;                 :expected 1/1}
;                {:description "the 1st day of a quarter"
;                 :budget {:period :quarter
;                          :period-count 4
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 1 1)
;                 :expected 1/91}
;                {:description "the 1st day of the 2nd month of a quarter"
;                 :budget {:period :quarter
;                          :period-count 4
;                          :start-date (t/local-date 2015 1 1)}
;                 :date (t/local-date 2015 2 1)
;                 :expected 16/45}
;                {:description "the last day of a quarter"
;                 :budget {:period :quarter
;                          :period-count 4
;                          :start-date (t/local-date 2016 1 1)}
;                 :date (t/local-date 2016 3 31)
;                 :expected 1/1}]]
;     (doseq [{:keys [description budget date expected]} tests]
;       (testing description
;         (is (= expected (budgets/percent-of-period budget date)))))))
; 
; (def ^:private get-items-context
;   (-> budget-context
;       (update-in [:accounts] concat [{:name "Food"
;                                       :parent-id "Groceries"
;                                       :type :expense}
;                                      {:name "Non-food"
;                                       :parent-id "Groceries"
;                                       :type :expense}])
;       (assoc :budgets [{:entity-id "Personal"
;                         :name "2015"
;                         :start-date (t/local-date 2015 1 1)
;                         :period-count 3
;                         :period :month
;                         :items [{:account-id "Food"
;                                  :periods (repeat 3 100M)}
;                                 {:account-id "Non-food"
;                                  :periods (repeat 3 50M)}]}])))
; 
; (deftest get-items-by-account
;   (let [ctx (realize get-items-context)
;         budget (find-budget ctx "2015")]
;     (testing "a leaf account"
;       (let [account (find-account ctx "Food")
;             expected [[100M 100M 100M]]
;             actual (map :periods (budgets/find-items-by-account
;                                   budget
;                                   account))]
;         (is (= expected actual) "The correct period values are returned")))
;     (testing "a parent account"
;       (let [account (find-account ctx "Groceries")
;             expected [[100M 100M 100M]
;                       [50M 50M 50M]]
;             actual (map :periods (budgets/find-items-by-account
;                                   budget
;                                   account))]
;         (is (= expected actual) "The correct period values are returned")))))
