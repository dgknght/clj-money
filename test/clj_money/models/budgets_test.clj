(ns clj-money.models.budgets-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.validation :as validation]
            [clj-money.models.budgets :as budgets]
            [clj-money.test-context :refer [realize
                                            basic-context
                                            find-entity
                                            find-account
                                            find-accounts
                                            find-budget]]
            [clj-money.test-helpers :refer [pprint-diff
                                            reset-db
                                            selective=
                                            assert-validation-error]]))

(use-fixtures :each (partial reset-db (env :db)))

(def budget-context
  basic-context)

(defn- attributes
  [ctx]
  (let [entity (find-entity ctx "Personal")
        salary (find-account ctx "Salary")
        groceries (find-account ctx "Groceries")
        rent (find-account ctx "Rent")]
    {:entity-id (:id entity)
     :name "2016"
     :start-date (t/local-date 2016 1 1)
     :period :month
     :period-count 3
     :items [{:account-id (:id salary)
              :periods [1000M 1001M 1002M]}
             {:account-id (:id rent)
              :periods [500M 500M 500M]}
             {:account-id (:id groceries)
              :periods [100M 90M 105M]}]}))

(deftest create-a-budget
  (let [ctx (realize (env :db) budget-context)
        entity (find-entity ctx "Personal")
        [salary groceries rent]  (find-accounts ctx "Salary" "Groceries" "Rent")
        budget (budgets/create
                 (env :db)
                 (attributes ctx))
        retrieved (budgets/find-by (env :db)
                                   {:entity-id (:id entity)}
                                   {:include-items? true})]
    (is (not (nil? (:id budget))) "The returned value has an id")
    (is (selective= {:name "2016"
                     :start-date (t/local-date 2016 1 1)
                     :period :month
                     :period-count 3}
                 retrieved)
        "The budget record can be retrieved.")
    (is (= 3 (-> retrieved :items count))
        "The retrieved budget has the correct number of items")
    (is (= [1000M 1001M 1002M]
           (->> (:items retrieved)
                (filter #(= (:id salary) (:account-id %)))
                (map :periods)
                first))
        "The salary item can be retrieved")
    (is (some #(selective= {:account-id (:id groceries)
                            :periods [100M 90M 105M]}
                           %)
              (:items retrieved))
        "The groceries item can be retrieved")
    (is (some #(selective= {:account-id (:id rent )
                            :periods [500M 500M 500M]}
                           %)
              (:items retrieved))
        "The rent item can be retrieved")))

(deftest entity-id-is-required
  (let [context (realize (env :db) budget-context)]
    (assert-validation-error
      :entity-id
      "Entity id is required"
      (budgets/create (env :db) (dissoc (attributes context) :entity-id)))))

(deftest name-is-required
  (let [context (realize (env :db) budget-context)
        result (budgets/create (env :db) (dissoc (attributes context) :name))]
    (assert-validation-error
      :name
      "Name is required"
      result)))

(deftest start-date-is-required
  (let [context (realize (env :db) budget-context)
        result (budgets/create (env :db) (dissoc (attributes context) :start-date))]
    (assert-validation-error
      :start-date
      "Start date is required"
      result)))

(deftest period-is-required
  (let [context (realize (env :db) budget-context)
        result (budgets/create (env :db) (dissoc (attributes context) :period))]
    (assert-validation-error
      :period
      "Period is required"
      result)))

(deftest period-must-be-week-month-or-quarter
  (let [context (realize (env :db) budget-context)
        result (budgets/create (env :db) (assoc (attributes context) :period :not-a-period))]
    (assert-validation-error
      :period
      "Period must be one of: quarter, week, month"
      result)))

(deftest period-count-is-required
  (let [context (realize (env :db) budget-context)
        result (budgets/create (env :db) (dissoc (attributes context) :period-count))]
    (assert-validation-error
      :period-count
      "Period count is required"
      result)))

(deftest period-count-must-be-greater-than-zero
  (let [context (realize (env :db) budget-context)
        result (budgets/create (env :db) (assoc (attributes context) :period-count 0))]
    (assert-validation-error
      :period-count
      "Period count must be greater than zero"
      result)))

(def delete-context
  (assoc budget-context
         :budgets [{:name "2016"
                    :period :month
                    :period-count 12
                    :start-date (t/local-date 2016 1 1)
                    :items [{:account-id "Salary"
                             :periods (repeat 12 1000M)}
                            {:account-id "Rent"
                             :periods (repeat 12 500M)}
                            {:account-id "Groceries"
                             :periods (repeat 12 100M)}]}]))

(deftest delete-a-budget
  (let [context (realize (env :db) delete-context)
        budget (-> context :budgets first)
        entity (-> context :entities first)]
    (is (= 1
           (count (budgets/search (env :db) {:entity-id (:id entity)})))
        "The budget is returned before delete")
    (budgets/delete (env :db) budget)
    (is (= 0
           (count (budgets/search (env :db) {:entity-id (:id entity)})))
        "The budget is absent after delete")))

(deftest update-a-budget
  (let [ctx (realize (env :db) delete-context)
        budget (find-budget ctx "2016")
        salary (find-account ctx "Salary")
        updated (-> budget
                    (assoc :name "edited")
                    (assoc :start-date (t/local-date 2015 1 1))
                    (assoc-in [:items 0 :periods] (repeat 12 1100M)))
        result (budgets/update (env :db) updated)
        retrieved (budgets/find-by-id (env :db) (:id budget))]
    (is (empty? (validation/error-messages result))
        "The budget is valid")
    (is (= "edited" (:name result))
        "The returned value reflects the update")
    (is (= (t/local-date 2015 12 31)
           (:end-date result))
        "the returned value reflects the recalculated end date")
    (is (= (repeat 12 1100M)
           (->> (:items updated)
                (filter #(= (:id salary) (:account-id %)))
                (map :periods)
                first))
        "The returned value reflects the updated items")
    (is (= "edited" (:name retrieved))
        "The retrieved value reflects the updated")
    (is (= (t/local-date 2015 12 31)
           (:end-date retrieved))
        "The retrieved value reflects the recalculated end date")
    (is (= (repeat 12 1100M)
           (->> (:items retrieved)
                (filter #(= (:id salary)  (:account-id %)))
                (map :periods)
                first))
        "The retrieved value reflects the updated items")))

#_(deftest budget-item-requires-account-id
  (let [context (realize (env :db) budget-context)
        attr (update-in (attributes context)
                        [:items 0]
                        dissoc
                        :account-id)
        result (budgets/create (env :db) attr)]
    (assert-validation-error
      :account-id
      "Account id is required"
      result)))

#_(deftest budget-item-account-must-belong-to-budget-entity
  (let [context (realize (env :db) budget-context)
        account (find-account context "Sales")
        attributes (update-in (attributes context)
                              [:items 0]
                              assoc
                              :account-id
                              (:id account))
        result (budgets/create (env :db) attributes)]
    (assert-validation-error
      :account-id
      "Account must belong to the same entity as the budget"
      result)))

#_(deftest budget-item-has-same-period-count-as-budget
  (let [context (realize (env :db) budget-context)
        attributes (update-in (attributes context)
                              [:items 0]
                              assoc
                              :periods
                              [100M])
        result (budgets/create (env :db) attributes)]
    (assert-validation-error
      :periods
      "Number of periods must match the budget \"Period count\" value"
      result)))

;; Periods

(deftest get-a-budget-end-date
  (testing "a monthly budget"
    (is (= (t/local-date 2017 12 31)
           (budgets/end-date {:period :month
                              :period-count 12
                              :start-date (t/local-date 2017 1 1)}))
        "It returns the last date of the last month"))
  (testing "a weekly budget"
    (is (= (t/local-date 2017 2 4)
           (budgets/end-date {:period :week
                              :period-count 5
                              :start-date (t/local-date 2017 1 1)}))
        "It returns the last date of the last week")))

(deftest get-the-index-of-the-period-containing-a-date
  (let [all-tests [{:period :month
                    :tests [{:date (t/local-date 2017 1 1)
                             :expected 0}
                            {:date (t/local-date 2017 1 31)
                             :expected 0}
                            {:date (t/local-date 2017 2 1)
                             :expected 1}
                            {:date (t/local-date 2017 2 28)
                             :expected 1}
                            {:date (t/local-date 2017 3 1)
                             :expected 2}
                            {:date (t/local-date 2017 12 31)
                             :expected 11}
                            {:date (t/local-date 2016 12 31)
                             :expected nil}
                            {:date (t/local-date 2018 1 1)
                             :expected nil}]}
                   {:period :week
                    :tests [{:date (t/local-date 2017 1 1)
                             :expected 0}
                            {:date (t/local-date 2017 1 7)
                             :expected 0}
                            {:date (t/local-date 2017 1 8)
                             :expected 1}
                            {:date (t/local-date 2017 3 25)
                             :expected 11}
                            {:date (t/local-date 2016 12 31)
                             :expected nil}
                            {:date (t/local-date 2017 3 26)
                             :expected nil}]}]
        budget {:start-date (t/local-date 2017 1 1)
                :period-count 12}]
    (doseq [{:keys [period tests]} all-tests]
      (testing (format "period %s" period)
        (doseq [{:keys [date expected]} tests]
          (is (= expected (:index (budgets/period-containing
                                    (assoc budget :period period)
                                    date)))
              (format "Given a budget starting on 1/1/2017, %s produces %s" date expected)))))

    (testing "monthly budget"
      (is (= 3 (:index (budgets/period-containing
                         (assoc budget :period :month)
                         (t/local-date 2017 4 3))))
          "It returns the index of the period containing the date"))
    (testing "weekly budget"
      (is (= 3 (:index (budgets/period-containing
                         (assoc budget :period :week)
                         (t/local-date 2017 1 25))))
          "It returns the index of the period containing the date"))))

(deftest find-a-budget-by-date
  (let [context (realize (env :db) delete-context)
        entity-id (-> context :entities first :id)
        tests [{:description "before any budgets"
                :date (t/local-date 2015 12 31)
                :expected nil}
               {:description "start of a budget"
                :date (t/local-date 2016 1 1)
                :expected "2016"}]]
    (doseq [{:keys [expected date description]} tests]
      (testing description
        (is (= expected
               (->> date
                    (budgets/find-by-date (env :db) entity-id)
                    :name)))))))

(deftest calculate-a-percent-of-a-period
  (let [tests [{:description "the first day of a month"
                :budget {:period :month
                         :period-count 12
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected 1/31}
               {:description "the 15th day of a month"
                :budget {:period :month
                         :period-count 12
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 4 15)
                :expected 1/2}
               {:description "the last day of a month"
                :budget {:period :month
                         :period-count 12
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 31)
                :expected 1/1}
               {:description "the first day of a week"
                :budget {:period :week
                         :period-count 8
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected 1/7}
               {:description "the 4th day of a week"
                :budget {:period :week
                         :period-count 8
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 4)
                :expected 4/7}
               {:description "the last day of a week"
                :budget {:period :week
                         :period-count 8
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 7)
                :expected 1/1}
               {:description "the 1st day of a quarter"
                :budget {:period :quarter
                         :period-count 4
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 1 1)
                :expected 1/91}
               {:description "the 1st day of the 2nd month of a quarter"
                :budget {:period :quarter
                         :period-count 4
                         :start-date (t/local-date 2015 1 1)}
                :date (t/local-date 2015 2 1)
                :expected 16/45}
               {:description "the last day of a quarter"
                :budget {:period :quarter
                         :period-count 4
                         :start-date (t/local-date 2016 1 1)}
                :date (t/local-date 2016 3 31)
                :expected 1/1}]]
    (doseq [{:keys [description budget date expected]} tests]
      (testing description
        (is (= expected (budgets/percent-of-period budget date)))))))

(def ^:private get-items-context
  (-> budget-context
      (update-in [:accounts] concat [{:name "Food"
                                      :parent-id "Groceries"
                                      :type :expense}
                                     {:name "Non-food"
                                      :parent-id "Groceries"
                                      :type :expense}])
      (assoc :budgets [{:entity-id "Personal"
                        :name "2015"
                        :start-date (t/local-date 2015 1 1)
                        :period-count 3
                        :period :month
                        :items [{:account-id "Food"
                                 :periods (repeat 3 100M)}
                                {:account-id "Non-food"
                                 :periods (repeat 3 50M)}]}])))

(deftest get-items-by-account
  (let [ctx (realize (env :db) get-items-context)
        budget (find-budget ctx "2015")]
    (testing "a leaf account"
      (let [account (find-account ctx "Food")
            expected [[100M 100M 100M]]
            actual (map :periods (budgets/find-items-by-account
                                   budget
                                   account
                                   (env :db)))]
        (pprint-diff expected actual)
        (is (= expected actual) "The correct period values are returned")))
    (testing "a parent account"
      (let [account (find-account ctx "Groceries")
            expected [[100M 100M 100M]
                      [50M 50M 50M]]
            actual (map :periods (budgets/find-items-by-account
                                   budget
                                   account
                                   (env :db)))]
        (pprint-diff expected actual)
        (is (= expected actual) "The correct period values are returned")))))
