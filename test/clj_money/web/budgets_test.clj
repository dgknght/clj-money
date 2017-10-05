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
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def budget-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Checking"
               :type :asset
               :commodity-id "USD"}
              {:name "Salary"
               :type :income
               :commodity-id "USD"}
              {:name "Bonus"
               :type :income
               :commodity-id "USD"}
              {:name "Rent"
               :type :expense
               :commodity-id "USD"}
              {:name "Groceries"
               :type :expense
               :commodity-id "USD"}]
   :budgets [{:name "2017"
              :period :month
              :period-count 12
              :start-date (t/local-date 2017 1 1)
              :items [{:account-id "Salary"
                       :periods (repeat 12 2000M)}
                      {:account-id "Bonus"
                       :periods [0M 0M 1000M 0M 0M 0M 0M 0M 0M 0M 0M 0M]}
                      {:account-id "Rent"
                       :periods (repeat 12 800M)}
                      {:account-id "Groceries"
                       :periods (repeat 12 200M)}]}]})

(deftest prepare-budget-for-display
  (let [context (serialization/realize storage-spec budget-context)
        actual (with-authentication (-> context :users first)
                 (-> (budgets/for-display (-> context :budgets first :id))
                     (update-in [:items] #(map (fn [i] (dissoc i :id)) %))
                     (dissoc :id :created-at :updated-at :entity-id)))
        expected {:name "2017"
                  :period :month
                  :period-count 12
                  :start-date (t/local-date 2017 1 1)
                  :end-date (t/local-date 2017 12 31)
                  :items [{:caption "Income"
                           :style :header
                           :data [{:value 2000M}
                                  {:value 2000M}
                                  {:value 3000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}]}
                          {:caption "Bonus"
                           :style :data
                           :data [{:value 0M}
                                  {:value 0M}
                                  {:value 1000M}
                                  {:value 0M}
                                  {:value 0M}
                                  {:value 0M}
                                  {:value 0M}
                                  {:value 0M}
                                  {:value 0M}
                                  {:value 0M}
                                  {:value 0M}
                                  {:value 0M}]}
                          {:caption "Salary"
                           :style :data
                           :data [{:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}
                                  {:value 2000M}]}
                          {:caption "Expense"
                           :style :header
                           :data [{:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}]}
                          {:caption "Groceries"
                           :style :data
                           :data [{:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}
                                  {:value 200M}]}
                          {:caption "Rent"
                           :style :data
                           :data [{:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}
                                  {:value 800M}]}
                          {:caption "Net"
                           :style :summary
                           :data [{:value 1000M}
                                  {:value 1000M}
                                  {:value 2000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}
                                  {:value 1000M}]}]}]
    (is (= expected actual))))

(deftest get-a-budget-period-label
  (is (= "Feb 2017"
         (budgets/period-label {:start-date (t/local-date 2017 1 1)
                                :period :month
                                :period-count 12}
                               1))))
