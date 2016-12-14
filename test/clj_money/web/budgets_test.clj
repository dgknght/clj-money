(ns clj-money.web.budgets-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.web.budgets :as budgets]
            [clj-money.test-helpers :refer [reset-db]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(defn- ->budget-item-periods
  [amounts]
  (->> amounts
       (map bigdec)
       (map-indexed #(hash-map :index %1 :amount %2))))

(def budget-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :accounts [{:name "Checking"
               :type :asset }
              {:name "Salary"
               :type :income}
              {:name "Bonus"
               :type :income}
              {:name "Rent"
               :type :expense}
              {:name "Groceries"
               :type :expense}]
   :budgets [{:name "2017"
              :period :month
              :period-count 12
              :start-date (t/local-date 2017 1 1)
              :items [{:account-id "Salary"
                       :periods (->budget-item-periods (repeat 12 2000))}
                      {:account-id "Bonus"
                       :periods (->budget-item-periods [0 0 1000 0 0 0 0 0 0 0 0 0])}
                      {:account-id "Rent"
                       :periods (->budget-item-periods (repeat 12 800))}
                      {:account-id "Groceries"
                       :periods (->budget-item-periods (repeat 12 200))}]}]})

(deftest prepare-budget-for-display
  (let [context (serialization/realize storage-spec budget-context)
        actual (-> (budgets/for-display (-> context :budgets first :id))
                    (update-in [:items] #(map (fn [i] (dissoc i :id)) %))
                    (dissoc :id :created-at :updated-at :entity-id))
        expected {:name "2017"
                  :period :month
                  :period-count 12
                  :start-date (t/local-date 2017 1 1)
                  :items [{:caption "Income"
                           :style :header
                           :data [{:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 3000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}]}
                          {:caption "Bonus"
                           :style :data
                           :data [{:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}
                                  {:value (bigdec 0)}]}
                          {:caption "Salary"
                           :style :data
                           :data [{:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 2000)}]}
                          {:caption "Expense"
                           :style :header
                           :data [{:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}]}
                          {:caption "Groceries"
                           :style :data
                           :data [{:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}
                                  {:value (bigdec 200)}]}
                          {:caption "Rent"
                           :style :data
                           :data [{:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}
                                  {:value (bigdec 800)}]}
                          {:caption "Net"
                           :style :summary
                           :data [{:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 2000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}
                                  {:value (bigdec 1000)}]}]}]
    (is (= expected actual))))

(deftest get-a-budget-period-label
  (is (= "Feb 2017"
         (budgets/period-label {:start-date (t/local-date 2017 1 1)
                                :period :month
                                :period-count 12}
                               1))))
