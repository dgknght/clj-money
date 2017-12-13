(ns clj-money.reports
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [pprint-and-return format-date]]
            [clj-money.inflection :refer [humanize]]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]))

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

(defn- default-commodity?
  [entity commodity]
  (if (= :currency (:type commodity))
    (if-let [id (-> entity :settings :default-commodity-id)]
      (= id (:id commodity))
      true))) ; assume default commodity id will be set if there is more than one commodity

(defn- account-value-as-of
  [storage-spec entity account as-of]
  (let [commodity (commodities/find-by-id storage-spec (:commodity-id account))
        price (if (default-commodity? entity commodity)
                1M
                (:price (prices/most-recent storage-spec (:id commodity) as-of)))
        shares (transactions/balance-as-of storage-spec (:id account) as-of)]
    (* shares price)))

(declare set-balances)
; TODO combine this with set-balance-delta
(defn- set-balance
  [storage-spec entity account as-of]
  (let [children (set-balances storage-spec entity as-of (:children account))
        new-children-balance (reduce #(+ %1
                                         (:balance %2)
                                         (:children-balance %2))
                                     0
                                     children)
        new-balance (account-value-as-of storage-spec entity account as-of)]
    (assoc account :children children
                   :children-balance new-children-balance
                   :balance new-balance)))

; TODO consider removing this function
(defn- set-balances
  [storage-spec entity as-of accounts]
  (map #(set-balance storage-spec entity % as-of) accounts))

(defn- set-balances-in-account-groups
  [storage-spec entity as-of groups]
  (mapv #(set-balances-in-account-group
           %
           (partial set-balances storage-spec entity as-of))
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
   (->> {:entity-id entity-id}
        (accounts/search storage-spec)
        (accounts/nest [:income :expense])
        (into [])
        (set-balance-deltas-in-account-groups storage-spec start end)
        transform-income-statement)))

(defn- transform-balance-sheet
  "Accepts group accounts and returns a report structure"
  [groups]
  (let [summary (->> groups
                     (map (juxt :type :value))
                     (into {}))
        retained (- (:income summary) (:expense summary))
        records (->> groups
                     (remove #(#{:income :expense} (:type %)))
                     (mapcat transform-account-group))]
    (concat records [{:caption "Liabilities + Equity"
                      :value (+ (:equity summary) (:liability summary))
                      :style :summary}])))

(defn- append-equity-pseudo-account
  [account-groups caption value]
  (update-in account-groups
             [2]
             (fn [entry]
               (-> entry
                   (update-in [:accounts] #(concat %
                                                   [{:name caption
                                                     :balance value
                                                     :children-balance 0}]))
                   (update-in [:value] #(+ % value))))))

(def ^:private pseudo-accounts
  [{:positive-caption "Retained Earnings"
    :negative-caption "Retained Losses"
    :calc-fn #(let [summary (->> (:account-groups %)
                             (map (juxt :type :value))
                             (into {}))]
            (- (:income summary) (:expense summary))) }
   {:positive-caption "Unrealized Gains"
    :negative-caption "Unrealized Losses"
    :calc-fn #(lots/unrealized-gains (:storage-spec %)
                                (:entity-id %)
                                (:as-of %))}])

(defn- append-pseudo-accounts
  [storage-spec entity-id as-of account-groups]
  (reduce (fn [result {:keys [calc-fn negative-caption positive-caption]}]
            (let [value (calc-fn {:storage-spec storage-spec
                                  :entity-id entity-id
                                  :as-of as-of
                                  :account-groups result})
                  caption (if (< value 0)
                            negative-caption
                            positive-caption)]
              (append-equity-pseudo-account result caption value)))
          account-groups
          pseudo-accounts))

(defn balance-sheet
  "Returns the data used to populate a balance sheet report"
  ([storage-spec entity-id]
   (let [base (t/today)
         end (t/local-date (t/year base) (t/month base) (t/number-of-days-in-the-month base))]
     (balance-sheet storage-spec entity-id end)))
  ([storage-spec entity-id as-of]
   (let [entity (entities/find-by-id storage-spec entity-id)]
     (->> {:entity-id entity-id}
          (accounts/search storage-spec)
          accounts/nest
          (set-balances-in-account-groups storage-spec entity as-of)
          (append-pseudo-accounts storage-spec entity-id as-of)
          transform-balance-sheet))))

(defn- ->budget-report-record
  [storage budget period-count as-of account]
  (let [item (budgets/find-item-by-account budget account)
        budget-amount (if item
                        (reduce + 0M (->> item
                                          :periods
                                          (take period-count)))
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
       :percent-difference (when (not= budget 0M)
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
    (if-let [budget (if (map? budget-or-id)
                      budget-or-id
                      (budgets/find-by-id s budget-or-id))]
      (let [period-count (+ 1 (:index (budgets/period-containing budget as-of)))
            items (->> {:entity-id (:entity-id budget)
                        :type #{:income :expense} }
                       (accounts/search s)
                       (group-by :type)
                       (sort-by  #(.indexOf [:income :expense] (first %)))
                       (mapcat #(process-budget-group s budget period-count as-of %))
                       (remove #(= 0M (:actual %) (:budget %)))
                       (append-summary period-count))]
        (-> budget
            (assoc :items items)
            (rename-keys {:name :title})
            (select-keys [:items :title])))
      [])))

(defn- monitor-item
  [budget actual percentage]
  {:total-budget budget
   :actual actual
   :percentage percentage
   :prorated-budget (* percentage budget)
   :actual-percent (/ actual budget)})

(defn monitor
  "Returns a mini-report for a specified account against a budget period"
  ([storage-spec account]
   (monitor storage-spec account {}))
  ([storage-spec account {as-of :as-of :or {as-of (t/today)}}]
   (with-storage [s storage-spec]
     (if-let [budget (budgets/find-by-date s (:entity-id account) as-of)]
       (if-let [item (budgets/find-item-by-account budget account)]
         (let [period (budgets/period-containing budget as-of)
               period-budget (get (:periods item) (:index period))
               total-budget (reduce + (->> item
                                            :periods))
               percent-of-period (budgets/percent-of-period budget
                                                            as-of)
               period-actual (transactions/balance-delta s
                                                         (:id account)
                                                         (:start period)
                                                         as-of)
               total-actual (transactions/balance-delta s
                                                        (:id account)
                                                        (:start-date budget)
                                                        as-of)
               percent-of-total (->> [as-of (:end-date budget)]
                                     (map tc/to-date-time)
                                     (map #(t/interval (tc/to-date-time (:start-date budget)) %))
                                     (map t/in-days)
                                     (map #(+ 1 %))
                                     (apply /))]
           (with-precision 5
             {:caption (:name account)
              :account account
              :period (monitor-item period-budget period-actual percent-of-period)
              :budget (monitor-item total-budget total-actual percent-of-total)}))
         {:caption (:name account)
          :account account
          :message "There is no budget item for this account"})
       {:caption (:name account)
        :account account
        :message (format "There is no budget for %s" (format-date as-of))}))))

(defn- summarize-commodity
  [storage [commodity-id lots]]
  (let [commodity (commodities/find-by-id storage commodity-id)
        shares (->> lots
                    (map :shares-owned)
                    (reduce +))
        cost (->> lots
                  (map #(* (:shares-owned %) (:purchase-price %)))
                  (reduce +))
        price (:price (prices/most-recent storage (:id commodity)))
        value (* price shares)
        gain (- value cost)]
    {:caption (format "%s (%s)" (:name commodity) (:symbol commodity))
     :commodity-id (:id commodity)
     :style :data
     :shares shares
     :price price
     :cost cost
     :value value
     :gain gain}))

(defn commodities-account-summary
  ([storage-spec account-id]
   (commodities-account-summary storage-spec account-id (t/today)))
  ([storage-spec account-id as-of]
   (with-storage [s storage-spec]
     (let [data (conj (->> {:account-id account-id}
                           (lots/search s)
                           (filter #(not= 0M (:shares-owned %)))
                           (group-by :commodity-id)
                           (map #(summarize-commodity s %))
                           (sort-by :caption)
                           (into []))
                      {:caption "Cash"
                       :style :data
                       :value (transactions/balance-as-of
                                s
                                account-id
                                as-of)})
           summary (reduce (fn [result record]
                             (reduce (fn [r k]
                                       (update-in r [k] #(+ % (or (k record) 0M))))
                                     result
                                     [:cost :value :gain]))
                           {:caption "Total"
                            :style :summary
                            :cost 0M
                            :value 0M
                            :gain 0M}
                           data)]
       (conj data summary)))))

(defn- append-commodity-caption
  [storage-spec {commodity-id :commodity-id :as lot}]
  (let [{:keys [name symbol]} (commodities/find-by-id storage-spec
                                                      commodity-id)]
    (assoc lot :caption (format "%s (%s)" name symbol))))

(defn- append-current-price
  [storage-spec lot]
  (assoc lot :current-price (->> (:commodity-id lot)
                                 (prices/most-recent storage-spec)
                                 :price)))

(defn- transform-lot-transactions
  [trans]
  (->> (:lot-items trans)
       (map (fn [item]
              (merge item (select-keys trans [:id :transaction-date]))))))

(defn- append-lot-transactions
  [storage-spec lot]
  (assoc lot
         :transactions
         (->> {:lot-id (:id lot)
               :transaction-date [:between
                                  (:purchase-date lot)
                                  (t/today)]}
              (transactions/search storage-spec)
              (mapcat transform-lot-transactions))))

(defn- append-lot-calculated-values
  [storage-spec lot]
  (let [cost (* (:shares-owned lot) (:purchase-price lot))
        value (* (:shares-owned lot) (:current-price lot))
        gain (- value cost)]
    (assoc lot
           :cost cost
           :value value
           :gain gain)))

(defn lot-report
  ([storage-spec account-id]
   (lot-report storage-spec account-id nil))
  ([storage-spec account-id commodity-id]
   (->> (cond-> {:account-id account-id}
          commodity-id
          (assoc :commodity-id commodity-id))
        (lots/search storage-spec)
        (map #(->> %
                   (append-commodity-caption storage-spec)
                   (append-current-price storage-spec)
                   (append-lot-transactions storage-spec)
                   (append-lot-calculated-values storage-spec)))
        (sort-by :caption)
        (map #(dissoc % :id :shares-purchased :updated-at :created-at :account-id)))))
