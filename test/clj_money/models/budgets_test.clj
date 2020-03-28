(ns clj-money.models.budgets-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.validation :as validation]
            [clj-money.models.budgets :as budgets]
            [clj-money.test-context :refer [realize]]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def base-context
  {:users [{:email "john@doe.com"
            :first-name "John"
            :last-name "Doe"
            :password "please01"}]
   :entities [{:user-id "john@doe.com"
               :name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency
                  :entity-id "Personal"}]})

(def budget-context
  base-context)

(defn- attributes
  [context]
  {:entity-id (-> context :entities first :id)
   :name "2017"
   :start-date (t/local-date 2017 1 1)
   :period :month
   :period-count 12})

(deftest create-a-budget
  (let [context (realize storage-spec budget-context)
        [entity] (:entities context)
        budget (budgets/create
                 storage-spec
                 (attributes context))
        budgets (budgets/search storage-spec {:entity-id (:id entity)})]
    (is (not (nil? (:id budget))) "The returned value has an id")
    (is (= ["2017"] (map :name budgets))
        "A query for budgets returns the new budget")))

(deftest entity-id-is-required
  (let [context (realize storage-spec budget-context)]
    (assert-validation-error
      :entity-id
      "Entity id is required"
      (budgets/create storage-spec (dissoc (attributes context) :entity-id)))))

(deftest name-is-required
  (let [context (realize storage-spec budget-context)
        result (budgets/create storage-spec (dissoc (attributes context) :name))]
    (assert-validation-error
      :name
      "Name is required"
      result)))

(deftest start-date-is-required
  (let [context (realize storage-spec budget-context)
        result (budgets/create storage-spec (dissoc (attributes context) :start-date))]
    (assert-validation-error
      :start-date
      "Start date is required"
      result)))

(deftest start-date-can-be-a-string
  (let [context (realize storage-spec budget-context)
        result (budgets/create storage-spec (assoc (attributes context) :start-date "3/2/2016"))]
    (is (empty? (validation/error-messages result)) "The budget should be valid with a string start date.")
    (is (= (t/local-date 2016 3 2) (:start-date result)) "The result should have the correct start date value")))

(deftest period-is-required
  (let [context (realize storage-spec budget-context)
        result (budgets/create storage-spec (dissoc (attributes context) :period))]
    (assert-validation-error
      :period
      "Period is required"
      result)))

(deftest period-must-be-week-month-or-quarter
  (let [context (realize storage-spec budget-context)
        result (budgets/create storage-spec (assoc (attributes context) :period :not-a-period))]
    (assert-validation-error
      :period
      "Period must be one of: quarter, week, month"
      result)))

(deftest period-count-is-required
  (let [context (realize storage-spec budget-context)
        result (budgets/create storage-spec (dissoc (attributes context) :period-count))]
    (assert-validation-error
      :period-count
      "Period count is required"
      result)))

(deftest period-count-must-be-greater-than-zero
  (let [context (realize storage-spec budget-context)
        result (budgets/create storage-spec (assoc (attributes context) :period-count 0))]
    (assert-validation-error
      :period-count
      "Period count must be greater than zero"
      result)))

(def delete-context
  (merge base-context
         {:budgets [{:name "2017"
                     :period :month
                     :period-count 12
                     :start-date (t/local-date 2017 1 1)}]}))

(deftest delete-a-budget
  (let [context (realize storage-spec delete-context)
        budget (-> context :budgets first)
        entity (-> context :entities first)]
    (is (= 1
           (count (budgets/search storage-spec {:entity-id (:id entity)})))
        "The budget is returned before delete")
    (budgets/delete storage-spec budget)
    (is (= 0
           (count (budgets/search storage-spec {:entity-id (:id entity)})))
        "The budget is absent after delete")))

(def update-context
  (merge base-context
         {:budgets [{:name "2017"
                     :period :month
                     :period-count 12
                     :start-date (t/local-date 2017 1 1)}]}))

(deftest update-a-budget
  (let [context (realize storage-spec update-context)
        budget (-> context :budgets first)
        updated (-> budget
                    (assoc :name "edited")
                    (assoc :start-date (t/local-date 2016 1 1)))
        result (budgets/update storage-spec updated)
        retrieved (budgets/find-by-id storage-spec (:id budget))]
    (is (empty? (validation/error-messages result))
        "The budget is valid")
    (is (= "edited" (:name result))
        "The returned value reflects the update")
    (is (= (t/local-date 2016 12 31)
           (:end-date result))
        "the returned value reflects the recalculated end date")
    (is (= "edited" (:name retrieved))
        "The retrieved value reflects the updated")
    (is (= (t/local-date 2016 12 31)
           (:end-date retrieved))
        "The retrieved value reflects the recalculated end date")))

;; Items
(def budget-item-context
  (-> base-context
      (update-in [:entities] #(conj % {:name "Business"}))
      (update-in [:commodities] #(conj % {:name "US Dollar"
                                          :symbol "USD"
                                          :entity-id "Business"
                                          :type :currency}))
      (merge {:accounts [{:name "Salary"
                          :type :income}
                         {:name "Rent"
                          :type :expense}
                         {:name "Groceries"
                          :type :expense}
                         {:name "Sales"
                          :type :income
                          :entity-id "Business"}]
              :budgets [{:name "2017"
                         :start-date (t/local-date 2017 1 1)
                         :period :month
                         :period-count 12}]})))

(defn- budget-item-attributes
  [context]
  {:account-id (-> context :accounts second :id)
   :budget-id (-> context :budgets first :id)
   :periods (repeat 12 700M)})

(deftest create-budget-item
  (let [context (realize storage-spec budget-item-context)
        item (budgets/create-item storage-spec (budget-item-attributes context))
        budget (budgets/reload storage-spec (-> context :budgets first))]
    (is (empty? (validation/error-messages item)) "The new item is valid")
    (is (not (nil? (:id item))) "The new item has an id value")
    (is (= 1 (-> budget :items count)) "The item is returned with the budget after create")))

(deftest budget-item-requires-budget-id
  (let [context (realize storage-spec budget-item-context)
        item (budgets/create-item storage-spec (-> context
                                                   budget-item-attributes
                                                   (dissoc :budget-id)))]
    (assert-validation-error
      :budget-id
      "Budget id is required"
      item)))

(deftest budget-item-requires-account-id
  (let [context (realize storage-spec budget-item-context)
        item (budgets/create-item storage-spec (-> context
                                                   budget-item-attributes
                                                   (dissoc :account-id)))]
    (assert-validation-error
      :account-id
      "Account id is required"
      item)))

(deftest budget-item-account-must-belong-to-budget-entity
  (let [context (realize storage-spec budget-item-context)
        attributes (-> context
                       budget-item-attributes
                       (assoc :account-id (-> context
                                              :accounts
                                              last
                                              :id)))
        item (budgets/create-item storage-spec attributes)]
    (assert-validation-error
      :account-id
      "Account must belong to the same entity as the budget"
      item)))

(deftest budget-item-has-same-period-count-as-budget
  (let [context (realize storage-spec budget-item-context)
        attributes (-> context
                       budget-item-attributes
                       (update-in [:periods] #(conj % 1M)))
        item (budgets/create-item storage-spec attributes)]
    (assert-validation-error
      :periods
      "Number of periods must match the budget \"Period count\" value"
      item)))

(def update-budget-item-context
  (merge base-context
         {:accounts [{:name "Salary"
                      :type :income}
                     {:name "Rent"
                      :type :expense}]
          :budgets [{:name "2017"
                     :period :month
                     :period-count 12
                     :start-date (t/local-date 2017 1 1)
                     :items [{:account-id "Salary"
                              :periods (repeat 12 2000M)}
                             {:account-id "Rent"
                              :periods (repeat 12 850M)}]}]}))

(deftest update-a-budget-item
  (let [context (realize storage-spec update-budget-item-context)
        [_ rent] (:accounts context)
        budget (-> context :budgets first)
        budget-item (->> budget
                         :items
                         (filter #(= (:id rent) (:account-id %)))
                         first)
        updated (assoc-in budget-item [:periods 0] 950M)
        result (budgets/update-item storage-spec updated)
        retrieved (->> budget
                       (budgets/reload storage-spec)
                       :items
                       (filter #(= (:id rent) (:account-id %)))
                       first)]
    (is (empty? (validation/error-messages result)) "The item is valid after update")
    (is (= [950M 850M 850M] (->> result :periods (take 3))) "The returned value contains the updates")
    (is (= [950M 850M 850M] (->> retrieved :periods (take 3))) "The retreived value contains the updates")))

;; Periods

;TODO Need to clean up validation error structure for these tests
#_(deftest budget-item-period-requires-amount
  (let [context (realize storage-spec budget-item-context)
        attributes (-> context
                       budget-item-attributes
                       (update-in [:periods 0] #(dissoc % :amount)))
        item (budgets/create-item storage-spec attributes)]
    (assert-validation-error
      :periods
      [{:amount "Amount is required"} nil nil nil nil nil nil nil nil nil nil nil]
      item)))

#_(deftest budget-item-period-requires-index
  (let [context (realize storage-spec budget-item-context)
        attributes (-> context
                       budget-item-attributes
                       (update-in [:periods 0] #(dissoc % :index)))
        item (budgets/create-item storage-spec attributes)]
    (assert-validation-error
      :periods
      [{:index "Index is required"} nil nil nil nil nil nil nil nil nil nil nil]
      item)))

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

(def find-by-date-context
  (merge base-context
         {:budgets [{:name "2016"
                     :period :month
                     :period-count 12
                     :start-date (t/local-date 2016 1 1)}
                    {:name "2017"
                     :period :month
                     :period-count 12
                     :start-date (t/local-date 2017 1 1)}]}))

(deftest find-a-budget-by-date
  (let [context (realize storage-spec find-by-date-context)
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
                    (budgets/find-by-date storage-spec entity-id)
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

(def monitor-context
  (merge base-context
         {:accounts [{:name "Salary"
                      :type :income}
                     {:name "Dining"
                      :type :expense}]}))
