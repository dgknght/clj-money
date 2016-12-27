(ns clj-money.models.budgets-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.serialization :as serialization]
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
               :name "Personal"}]})

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
  (let [context (serialization/realize storage-spec budget-context)
        [entity] (:entities context)
        budget (budgets/create
                 storage-spec
                 (attributes context))
        budgets (budgets/select-by-entity-id storage-spec (:id entity))]
    (is (not (nil? (:id budget))) "The returned value has an id")
    (is (= ["2017"] (map :name budgets))
        "A query for budgets returns the new budget")))

(deftest entity-id-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)]
    (assert-validation-error
      :entity-id
      "Entity id is required"
      (budgets/create storage-spec (dissoc (attributes context) :entity-id)))))

(deftest name-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :name))]
    (assert-validation-error
      :name
      "Name is required"
      result)))

(deftest start-date-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :start-date))]
    (assert-validation-error
      :start-date
      "Start date is required"
      result)))

(deftest start-date-can-be-a-string
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (assoc (attributes context) :start-date "3/2/2016"))]
    (is (validation/valid? result) "The budget should be valid with a string start date.")
    (is (= (t/local-date 2016 3 2) (:start-date result)) "The result should have the correct start date value")))

(deftest period-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :period))]
    (assert-validation-error
      :period
      "Period is required"
      result)))

(deftest period-must-be-week-month-or-quarter
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (assoc (attributes context) :period :not-a-period))]
    (assert-validation-error
      :period
      "Period must be one of: quarter, week, month"
      result)))

(deftest period-count-is-required
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
        result (budgets/create storage-spec (dissoc (attributes context) :period-count))]
    (assert-validation-error
      :period-count
      "Period count is required"
      result)))

(deftest period-count-must-be-greater-than-zero
  (let [context (serialization/realize storage-spec budget-context)
        entity (-> context :entities first)
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
  (let [context (serialization/realize storage-spec delete-context)
        budget (-> context :budgets first)]
    (is (= 1
           (count (budgets/select-by-entity-id storage-spec (-> context :entities first :id))))
        "The budget is returned before delete")
    (budgets/delete storage-spec (:id budget))
    (is (= 0
           (count (budgets/select-by-entity-id storage-spec (-> context :entities first :id))))
        "The budget is absent after delete")))

(def update-context
  (merge base-context
         {:budgets [{:name "2017"
                     :period :month
                     :period-count 12
                     :start-date (t/local-date 2017 1 1)}]}))

(deftest update-a-budget
  (let [context (serialization/realize storage-spec update-context)
        budget (-> context :budgets first)
        updated (budgets/update storage-spec (assoc budget :name "edited"))
        retrieved (budgets/find-by-id storage-spec (:id budget))]
    (is (validation/valid? updated)
        "The budget is valid")
    (is (= "edited" (:name updated))
        "The returned value reflects the update")
    (is (= "edited" (:name retrieved))
        "The retrieved value reflects the updated")))

;; Items
(def budget-item-context
  (-> base-context
      (update-in [:entities] #(conj % {:name "Business"}))
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
   :periods (mapv #(hash-map :amount 700M
                             :index %)
                  (range 12))})

(deftest create-budget-item
  (let [context (serialization/realize storage-spec budget-item-context)
        item (budgets/create-item storage-spec (budget-item-attributes context))
        budget (budgets/reload storage-spec (-> context :budgets first))]
    (is (validation/valid? item) "The new item is valid")
    (is (not (nil? (:id item))) "The new item has an id value")
    (is (= 1 (-> budget :items count)) "The item is returned with the budget after create")))

(deftest budget-item-requires-budget-id
  (let [context (serialization/realize storage-spec budget-item-context)
        item (budgets/create-item storage-spec (-> context
                                                   budget-item-attributes
                                                   (dissoc :budget-id)))]
    (assert-validation-error
      :budget-id
      "Budget id is required"
      item)))

(deftest budget-item-requires-account-id
  (let [context (serialization/realize storage-spec budget-item-context)
        item (budgets/create-item storage-spec (-> context
                                                   budget-item-attributes
                                                   (dissoc :account-id)))]
    (assert-validation-error
      :account-id
      "Account id is required"
      item)))

(deftest budget-item-account-must-belong-to-budget-entity
  (let [context (serialization/realize storage-spec budget-item-context)
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
  (let [context (serialization/realize storage-spec budget-item-context)
        attributes (-> context
                       budget-item-attributes
                       (update-in [:periods] #(conj % {:index 12
                                                       :amount 1M})))
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
                              :periods (map-indexed #(hash-map :index %1 :amount %2) (repeat 12 2000M))}
                             {:account-id "Rent"
                              :periods (map-indexed #(hash-map :index %1 :amount %2) (repeat 12 850M))}]}]}))

(deftest update-a-budget-item
  (let [context (serialization/realize storage-spec update-budget-item-context)
        [salary rent] (:accounts context)
        budget (-> context :budgets first)
        budget-item (->> budget
                         :items
                         (filter #(= (:id rent) (:account-id %)))
                         first)
        updated (assoc-in budget-item [:periods 0 :amount] 950M)
        result (budgets/update-item storage-spec updated)
        retrieved (->> budget
                       (budgets/reload storage-spec)
                       :items
                       (filter #(= (:id rent) (:account-id %)))
                       first)]
    (is (validation/valid? result) "The item is valid for update")
    (is (= [950M 850M 850M] (->> result :periods (take 3) (map :amount))) "The returned value contains the updates")
    (is (= [950M 850M 850M] (->> retrieved :periods (take 3) (map :amount))) "The retreived value contains the updates")))

;; Periods

;TODO Need to clean up validation error structure for these tests
#_(deftest budget-item-period-requires-amount
  (let [context (serialization/realize storage-spec budget-item-context)
        attributes (-> context
                       budget-item-attributes
                       (update-in [:periods 0] #(dissoc % :amount)))
        item (budgets/create-item storage-spec attributes)]
    (assert-validation-error
      :periods
      [{:amount "Amount is required"} nil nil nil nil nil nil nil nil nil nil nil]
      item)))

#_(deftest budget-item-period-requires-index
  (let [context (serialization/realize storage-spec budget-item-context)
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
          (is (= expected (budgets/period-containing
                            (assoc budget :period period)
                            date))
              (format "Given a budget starting on 1/1/2017, %s produces" date expected)))))

    (testing "monthly budget"
      (is (= 3 (budgets/period-containing
                 (assoc budget :period :month)
                 (t/local-date 2017 4 3)))
          "It returns the index of the period containing the date"))
    (testing "weekly budget"
      (is (= 3 (budgets/period-containing
                 (assoc budget :period :week)
                 (t/local-date 2017 1 25)))
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
  (let [context (serialization/realize storage-spec find-by-date-context)
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
  (testing "the first day of a month"
    (is (= 1/31
           (budgets/percent-of-period {:period :month
                                       :period-count 12
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 1 1)))))
  (testing "the 15th day of a month"
    (is (= 1/2
           (budgets/percent-of-period {:period :month
                                       :period-count 12
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 4 15)))))
  (testing "the last day of a month"
    (is (= 1/1
           (budgets/percent-of-period {:period :month
                                       :period-count 12
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 1 31)))))
  (testing "the first day of a week"
    (is (= 1/7
           (budgets/percent-of-period {:period :week
                                       :period-count 8
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 1 1)))))
  (testing "the 4th day of a week"
    (is (= 4/7
           (budgets/percent-of-period {:period :week
                                       :period-count 8
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 1 4)))))
  (testing "the last day of a week"
    (is (= 1/1
           (budgets/percent-of-period {:period :week
                                       :period-count 8
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 1 7)))))
  (testing "the 1st day of a quarter"
    (is (= 1/91
           (budgets/percent-of-period {:period :quarter
                                       :period-count 4
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 1 1)))))
  (testing "the 1st day of the 2nd month of a quarter"
    (is (= 16/45
           (budgets/percent-of-period {:period :quarter
                                       :period-count 4
                                       :start-date (t/local-date 2015 1 1)}
                                      (t/local-date 2015 2 1)))))
  (testing "the last day of a quarter"
    (is (= 1/1
           (budgets/percent-of-period {:period :quarter
                                       :period-count 4
                                       :start-date (t/local-date 2016 1 1)}
                                      (t/local-date 2016 3 31))))))
