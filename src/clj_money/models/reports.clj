(ns clj-money.models.reports
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-time.core :as t]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.inflection :refer [humanize]]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]))

(declare set-balance-deltas)
(defn- set-balance-delta
  [storage-spec account start end]
  (let [children (set-balance-deltas storage-spec start end (:children account))
        new-children-balance (reduce #(+ %1
                                         (:balance %2)
                                         (:children-balance %2))
                                     0
                                     children)
        new-balance (transactions/balance-delta storage-spec
                                                (:id account)
                                                start
                                                end)]
    (assoc account :children children
                   :children-balance new-children-balance
                   :balance new-balance)))

; TODO consider removing this function
(defn- set-balance-deltas
  [storage-spec start end accounts]
  (map #(set-balance-delta storage-spec % start end) accounts))

(defn- set-balances-in-account-group
  [account-group calc-fn]
  (let [updated (update-in account-group [:accounts] calc-fn)]
    (assoc updated :value (reduce #(+ %1
                                      (:balance %2)
                                      (:children-balance %2))
                                  (bigdec 0)
                                  (:accounts updated)))))

(defn- set-balance-deltas-in-account-groups
  [storage-spec start end groups]
  (map #(set-balances-in-account-group
          %
          (partial set-balance-deltas storage-spec start end))
       groups))

(declare set-balances)
; TODO combine this with set-balance-delta
(defn- set-balance
  [storage-spec account as-of]
  (let [children (set-balances storage-spec as-of (:children account))
        new-children-balance (reduce #(+ %1
                                         (:balance %2)
                                         (:children-balance %2))
                                     0
                                     children)
        new-balance (transactions/balance-as-of storage-spec
                                                (:id account)
                                                as-of)]
    (assoc account :children children
                   :children-balance new-children-balance
                   :balance new-balance)))

; TODO consider removing this function
(defn- set-balances
  [storage-spec as-of accounts]
  (map #(set-balance storage-spec % as-of) accounts))

(defn- set-balances-in-account-groups
  [storage-spec as-of groups]
  (map #(set-balances-in-account-group
          %
          (partial set-balances storage-spec as-of))
       groups))

(defn- transform-account
  [account depth]
  (concat [{:caption (:name account)
            :value (+ (:balance account) (:children-balance account))
            :style :data
            :depth depth}]
          (mapcat #(transform-account % (+ 1 depth))
               (:children account))))

(defn- transform-account-group
  [{:keys [type accounts value]}]
  (concat [{:caption (humanize type)
            :value value
            :style :header}]
          (mapcat #(transform-account % 0) accounts)))

(defn- transform-income-statement
  "Accepts grouped accounts structure and returns a report structure"
  [groups]
  (let [summary (->> groups
                     (map (juxt :type :value))
                     (into {}))]
    (concat (mapcat transform-account-group
                    groups)
            [{:caption "Net"
              :value (- (:income summary) (:expense summary))
              :style :summary}])))

(defn income-statement
  "Returns the data used to populate an income statement report"
  ([storage-spec entity-id]
   (let [base (t/today)
         start (t/local-date (t/year base) 1 1)
         end (t/local-date (t/year base) (t/month base) (t/number-of-days-in-the-month base))]
     (income-statement storage-spec entity-id start end)))
  ([storage-spec entity-id start end]
   (->> (accounts/select-nested-by-entity-id
          storage-spec
          entity-id
          [:income :expense])
        (into [])
        (set-balance-deltas-in-account-groups storage-spec start end)
        transform-income-statement)))

(defn- transform-balance-sheet
  "Accepts group accounts and returns a report structure"
  [groups]
  (let [summary (->> groups
                     (map (juxt :type :value))
                     (into {}))
        retained (- (:income summary) (:expense summary))]
    (->> groups
         (map (fn [entry]
                (if (= :equity (:type entry))
                  (-> entry 
                      (update-in [:accounts] #(conj % {:name "Retained Earnings"
                                                       :balance retained
                                                       :children-balance 0}))
                      (update-in [:value] #(+ %1 retained)))
                  entry)))
         (remove #(#{:income :expense} (:type %)))
         (mapcat transform-account-group))))

(defn balance-sheet
  "Returns the data used to populate a balance sheet report"
  ([storage-spec entity-id]
   (let [base (t/today)
         end (t/local-date (t/year base) (t/month base) (t/number-of-days-in-the-month base))]
     (balance-sheet storage-spec entity-id end)))
  ([storage-spec entity-id as-of]
   (->> (accounts/select-nested-by-entity-id
          storage-spec
          entity-id)
        (into [])
        (set-balances-in-account-groups storage-spec as-of)
        transform-balance-sheet)))

(defn- ->budget-report-record
  [storage budget period-count as-of account]
  (let [item (->> (:items budget)
                  (filter #(= (:id account) (:account-id %)))
                  first)
        budget-amount (if item
                        (reduce + 0M (->> item
                                          :periods
                                          (take period-count)
                                          (map :amount)))
                        0M) ; TODO only total the periods up to and including the as-of date
        actual-amount (transactions/balance-delta storage (:id account)
                                                  (:start-date budget)
                                                  as-of)
        difference (if (accounts/left-side? account)
                     (- budget-amount actual-amount)
                     (- actual-amount budget-amount))]
    (with-precision 10
      {:caption (:name account)
       :style :data
       :budget budget-amount
       :actual actual-amount
       :difference difference
       :percent-difference (when (not= 0M budget-amount)
                             (/ difference budget-amount)) 
       :actual-per-period (/ actual-amount period-count)})))

(defn- budget-group-header
  [period-count account-type records]
  (let [budget (reduce + (map :budget records))
        actual (reduce + (map :actual records))
        difference (reduce + (map :difference records))]
    (with-precision 10
      {:caption (humanize account-type)
       :style :header
       :budget budget
       :actual actual
       :difference difference
       :percent-difference (when (not= budget 0)
                             (/ difference budget))
       :actual-per-period  (/ actual period-count)})))

(defn- process-budget-group
  [storage budget period-count as-of [account-type items]]
  (let [records (->> items
                     (map #(->budget-report-record storage budget period-count as-of %))
                     (sort-by :difference))]
    (conj records
          (budget-group-header period-count account-type records))))

(defn- append-summary
  [period-count records]
  (let [income (->> records
                    (filter #(= "Income" (:caption %)))
                    first)
        expense (->> records
                     (filter #(= "Expense" (:caption %)))
                     first)
        budget (->> [income expense]
                    (map #(:budget %))
                    (apply -))
        actual (->> [income expense]
                    (map #(:actual %))
                    (apply -))
        difference (- actual budget)]
    (with-precision 10
      (concat records [{:caption "Net"
                        :style :summary
                        :budget budget
                        :actual actual
                        :difference difference
                        :percent-difference (when (not= 0M budget)
                                              (/ difference budget))
                        :actual-per-period (/ actual period-count)}]))))

(defn budget
  "Returns a budget report"
  [storage-spec budget-or-id as-of]
  (with-storage [s storage-spec]
    (let [budget (if (map? budget-or-id)
                   budget-or-id
                   (budgets/find-by-id s budget-or-id))
          period-count (+ 1 (:index (budgets/period-containing budget as-of)))
          items (->> (accounts/select-by-entity-id s
                                                   (:entity-id budget)
                                                   {:types #{:income :expense}})
                     (group-by :type)
                     (sort-by  #(.indexOf [:income :expense] (first %)))
                     (mapcat #(process-budget-group s budget period-count as-of %))
                     (remove #(= 0M (:actual %) (:budget %)))
                     (append-summary period-count))]
      (-> budget
          (assoc :items items)
          (rename-keys {:name :title})
          (select-keys [:items :title])))))

(defn monitor
  "Returns a mini-report for a specified account against a budget period"
  ([storage-spec account]
   (monitor storage-spec account {}))
  ([storage-spec account options]
   (with-storage [s storage-spec]
     (let [as-of (or (:as-of options)
                     (t/today))
           budget (budgets/find-by-date s (:entity-id account) as-of)
           item (->> budget
                     :items
                     (filter #(= (:id account) (:account-id %)))
                     first)
           period-index (:index (budgets/period-containing budget as-of))
           total-budget (reduce + (->> item
                                       :periods
                                       (map :amount)
                                       (take (+ 1 period-index))))
           percent-of-period (budgets/percent-of-period budget
                                                        as-of)]
       {:account account
        :period {:total-budget total-budget
                 :prorated-budget (with-precision 5 (* percent-of-period total-budget))}
        :budget {}}))))
