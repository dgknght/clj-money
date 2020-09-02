(ns clj-money.reports
  (:require [clojure.string :as string]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]
            [stowaway.core :refer [with-storage]]
            [clj-money.util :refer [format-date]]
            [clj-money.inflection :refer [humanize]]
            [clj-money.models.date-helpers :refer [available-date-range]]
            [clj-money.models.accounts :as accounts]
            [clj-money.accounts :refer [nest
                                        unnest
                                        left-side?]]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]))

(defn- total-value-of-accounts
  [accounts]
  (->> accounts
       (mapcat (juxt :value :children-value))
       (remove nil?)
       (reduce + 0M)))

(declare set-balance-deltas)
(defn- set-balance-delta
  [account {:keys [storage start end] :as ctx}]
  (let [children (set-balance-deltas ctx (:children account))]
    (assoc account
           :children children
           :children-value (total-value-of-accounts children)
           :value (transactions/balance-delta storage
                                              account
                                              start
                                              end))))

(defn- set-balance-deltas
  [ctx accounts]
  (map #(set-balance-delta % ctx) accounts))

(defn- set-balances-in-account-group
  [account-group calc-fn]
  (let [updated (update-in account-group [:accounts] calc-fn)]
    (assoc updated :value (total-value-of-accounts (:accounts updated)))))

(defn- set-balance-deltas-in-account-groups
  [ctx groups]
  (map #(set-balances-in-account-group
          %
          (partial set-balance-deltas ctx))
       groups))

(defn- account-value-as-of
  [account {:keys [get-price storage as-of]}]
  (let [balance (or (transactions/balance-as-of storage account as-of)
                    0M)
        price (or (get-price (:commodity-id account))
                  0M)]
    (* balance price)))

(declare set-balances)
(defn- set-balance
  [account ctx]
  (let [children (set-balances ctx (:children account))]
    (assoc account
           :children children
           :children-value (total-value-of-accounts children)
           :value (account-value-as-of account ctx))))

(defn- set-balances
  [ctx accounts]
  (map #(set-balance % ctx) accounts))

(defn- set-balances-in-account-groups
  [ctx groups]
  (mapv #(set-balances-in-account-group
           %
           (partial set-balances ctx))
        groups))

(defn- transform-account
  [account depth]
  (concat [{:caption (:name account)
            :id (:id account)
            :value (+ (:value account) (:children-value account))
            :style :data
            :depth depth}]
          (mapcat #(transform-account % (inc depth))
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

(defn- remove-zero-values
  [records]
  (remove #(and (= 0M (:value %))
                      (= :data (:style %)))
          records))

(defn income-statement
  "Returns the data used to populate an income statement report"
  ([storage-spec entity]
   (let [[start end] (available-date-range)]
     (income-statement storage-spec entity start end)))
  ([storage-spec entity start end]
   (->> (accounts/search storage-spec
                         {:entity-id (:id entity)
                          :type ["income" "expense"]})
        (nest {:account-types [:income :expense]})
        (into [])
        (set-balance-deltas-in-account-groups
          {:storage storage-spec
           :start start
           :end end})
        transform-income-statement
        remove-zero-values)))

(defn- transform-balance-sheet
  "Accepts group accounts and returns a report structure"
  [groups]
  (let [summary (->> groups
                     (map (juxt :type :value))
                     (into {}))
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
                                                     :id (string/lower-case caption)
                                                     :value value
                                                     :children-value 0}]))
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
    :calc-fn #(lots/unrealized-gains (:storage %)
                                     (-> % :entity :id)
                                     (:as-of %))}])

(defn- append-pseudo-account
  [result {:keys [calc-fn negative-caption positive-caption]} ctx]
  (let [value (calc-fn (assoc ctx :account-groups result))
        caption (if (< value 0)
                  negative-caption
                  positive-caption)]
    (append-equity-pseudo-account result caption value)))

(defn- append-pseudo-accounts
  [ctx account-groups]
  (reduce #(append-pseudo-account %1 %2 ctx)
          account-groups
          pseudo-accounts))

(defn- default-commodity?
  [entity commodity]
  (when (= :currency (:type commodity))
    (if-let [id (-> entity :settings :default-commodity-id)]
      (= id (:id commodity))
      true))) ; assume default commodity id will be set if there is more than one commodity

(defn- get-price-fn
  [storage entity as-of]
  (let [cache (atom {:prices {}
                     :commodities (->> (commodities/search storage
                                                           {:entity-id (:id entity)})
                                       (map (juxt :id identity))
                                       (into {}))})]
    (fn [commodity-id]
      (if-let [price (get-in @cache [:prices commodity-id])]
        price
        (let [commodity (get-in @cache [:commodities commodity-id])
              price (if (default-commodity? entity commodity)
                      1M
                      (:price (prices/most-recent storage commodity as-of)))]
          (swap! cache assoc-in [:prices commodity-id] price)
          price)))))

(defn balance-sheet
  "Returns the data used to populate a balance sheet report"
  ([storage-spec entity]
   (balance-sheet storage-spec
                  entity
                  (t/today)))
  ([storage-spec entity as-of]
   (let [ctx {:storage storage-spec
              :entity entity
              :as-of as-of
              :get-price (get-price-fn storage-spec entity as-of)}]
     (->> (accounts/search storage-spec {:entity-id (:id entity)})
          nest
          (set-balances-in-account-groups ctx)
          (append-pseudo-accounts ctx)
          transform-balance-sheet
          remove-zero-values))))

(defn- ->budget-report-record
  [account storage budget {:keys [period-count as-of]}]
  (let [item (budgets/find-item-by-account budget account)
        budget-amount (if item
                        (reduce + 0M (->> item
                                          :periods
                                          (take period-count)))
                        0M) ; TODO only total the periods up to and including the as-of date
        actual-amount (transactions/balance-delta storage
                                                  account
                                                  (:start-date budget)
                                                  as-of)
        difference (if (left-side? account)
                     (- budget-amount actual-amount)
                     (- actual-amount budget-amount))]
    (with-precision 10
      {:id (:id account)
       :parent-id (:parent-id account)
       :caption (or (:path account)
                    (:name account))
       :style :data
       :budget budget-amount
       :actual actual-amount
       :difference difference
       :percent-difference (when (not= 0M budget-amount)
                             (with-precision 5
                               (/ difference budget-amount)))
       :actual-per-period (with-precision 5
                            (/ actual-amount period-count))})))

(defn- sum
  [attr col]
  (->> col
       (map attr)
       (reduce +)))

(defn- budget-group-header
  [period-count account-type records]
  (let [[budget actual diff] (map #(sum % records)
                                  [:budget :actual :difference])]
    (with-precision 10
      {:caption (humanize account-type)
       :style :header
       :budget budget
       :actual actual
       :difference diff
       :percent-difference (when-not (zero? budget)
                             (/ diff budget))
       :actual-per-period  (/ actual period-count)
       :items records})))

(defn- append-roll-up
  [{:keys [budget actual difference] :as record} children period-count]
  (let [b (+ budget (sum :budget children))
        a (+ actual (sum :actual children))
        d (+ difference (sum :difference children))]
    (assoc record
           :roll-up {:budget b
                     :actual a
                     :difference d
                     :percent-difference (when-not (= 0M b)
                                           (with-precision 10
                                             (/ d b)))
                     :actual-per-period (with-precision 10
                                          (/ a period-count))})))

(defn- roll-up*
  [record depth records-map period-count]
  (let [children (mapcat #(roll-up* % (inc depth) records-map period-count)
                         (get-in records-map [(:id record)] []))
        r (assoc record :depth depth)]
    (if (seq children)
      (conj children (append-roll-up r children period-count))
      [r])))

(defn- roll-up
  [period-count records]
  (let [records-map (reduce (fn [m r]
                              (update-in m [(:parent-id r)] (fnil conj []) r))
                            {}
                            records)]
    (->> records
         (remove :parent-id)
         (mapcat #(roll-up* % 0 records-map period-count)))))

(defn- zero-budget-report-item?
  [item]
  (every? #(= 0M (get-in item % 0M))
          [[:actual]
           [:budget]
           [:roll-up :actual]
           [:roll-up :budget]]))

(defn- process-budget-group
  [storage budget {:keys [period-count] :as options} [account-type accounts]]
  (budget-group-header period-count
                       account-type
                       (->> accounts
                            (map #(->budget-report-record % storage budget options))
                            (sort-by :difference)
                            (roll-up period-count)
                            (map #(dissoc % :parent-id))
                            (remove zero-budget-report-item?))))

(defn- append-summary
  [period-count records]
  (let [income (->> records
                    (filter #(= "Income" (:caption %)))
                    first)
        expense (->> records
                     (filter #(= "Expense" (:caption %)))
                     first)
        budget (if (and income expense)
                 (->> [income expense]
                      (map #(:budget %))
                      (apply -))
                 0M)
        actual (if (and income expense)
                 (->> [income expense]
                      (map #(:actual %))
                      (apply -))
                 0M)
        difference (- actual budget)]
    (with-precision 10
      (concat records [{:caption "Net"
                        :style :summary
                        :budget budget
                        :actual actual
                        :difference difference
                        :percent-difference (when-not (zero? budget)
                                              (/ difference budget))
                        :actual-per-period (/ actual period-count)}]))))

(defn- earlier
  [d1 d2]
  (->> [d1 d2]
       (filter identity)
       sort
       first))

(defn- end-of-last-month []
  (-> (t/today)
      (t/minus (t/months 1))
      t/last-day-of-the-month))

(defn- default-budget-end-date
  [bdg]
  (earlier (end-of-last-month) (:end-date bdg)))

(defn budget
  "Returns a budget report"
  ([storage-spec bdg]
   (budget storage-spec bdg {}))
  ([storage-spec budget {:keys [as-of] :as options}]
   (with-storage [s storage-spec]
     (let [as-of (or as-of
                     (default-budget-end-date budget))
           context (assoc options
                          :as-of as-of
                          :period-count (if (t/after? (:end-date budget)
                                                      as-of)
                                          (inc (:index (budgets/period-containing budget as-of)))
                                          (:period-count budget)))
           items (->> (accounts/search s {:entity-id (:entity-id budget)
                                          :type #{:income :expense}})
                      nest
                      unnest
                      (group-by :type)
                      (sort-by  #(.indexOf [:income :expense] (first %)))
                      (map #(process-budget-group s budget context %))
                      (append-summary (:period-count context)))]
       {:items items
        :title (format "%s: %s to %s"
                       (:name budget)
                       (tf/unparse-local-date (tf/formatter "MMMM") (:start-date budget))
                       (tf/unparse-local-date (tf/formatter "MMMM") as-of))}))))

(defn- monitor-item
  [budget actual percentage]
  {:total-budget budget
   :actual actual
   :percentage percentage
   :prorated-budget (* percentage budget)
   :actual-percent (/ actual budget)})

(defn- aggregate-account-actuals
  [accounts start end storage]
  (->> accounts
       (map #(transactions/balance-delta storage % start end))
       (reduce + 0M)))

(defn- monitor-from-item
  [{:keys [storage account as-of budget children] {:keys [periods]} :item}]
  (let [period (budgets/period-containing budget as-of)
        period-budget (nth periods (:index period) 0M)
        total-budget (reduce + 0M periods)
        percent-of-period (budgets/percent-of-period budget
                                                     as-of)
        period-actual (aggregate-account-actuals (conj children account)
                                                 (:start period)
                                                 as-of
                                                 storage)
        total-actual (aggregate-account-actuals (conj children account)
                                                (:start-date budget)
                                                as-of
                                                storage)
        percent-of-total (with-precision 5
                           (->> [as-of (:end-date budget)]
                                (map (comp inc
                                           t/in-days
                                           #(t/interval
                                              (tc/to-date-time (:start-date budget))
                                              %)
                                           tc/to-date-time))
                                (apply /)))]
    (with-precision 5
      {:caption (:name account)
       :account account
       :period (monitor-item period-budget period-actual percent-of-period)
       :budget (monitor-item total-budget total-actual percent-of-total)})))

(defn- aggregate-item
  [{:keys [budget account storage]}]
  (let [items (budgets/find-items-by-account budget account storage)]
    (when (seq items)
      {:account account
       :periods (->> items
                     (map :periods)
                     (apply interleave)
                     (partition (count items))
                     (map #(reduce + %)))})))

(defn- append-account-children
  [{:keys [account storage] :as ctx}]
  (let [children (remove
                   #(= (:id %)  (:id account))
                   (accounts/search storage
                                    {:id (:id account)}
                                    {:include-children? true}))]
    (-> ctx
        (update-in [:account] assoc :child-ids (map :id children))
        (assoc :children children))))

(defn- monitor-from-budget
  [{:keys [account] :as ctx}]
  (if-let [item (aggregate-item ctx)]
    (monitor-from-item (-> ctx
                           (assoc :item item)
                           append-account-children))
    {:caption (:name account)
     :account account
     :message "There is no budget item for this account"}))

(defn monitor
  "Returns a mini-report for a specified account against a budget period"
  ([storage-spec account]
   (monitor storage-spec account (t/today)))
  ([storage-spec account as-of]
   (with-storage [s storage-spec]
     (if-let [budget (budgets/find-by-date s (:entity-id account) as-of)]
       (monitor-from-budget {:storage s
                             :account account
                             :as-of as-of
                             :budget budget})
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
                  (map #(* (:shares-owned %)
                           (:purchase-price %)))
                  (reduce +))
        price (:price (prices/most-recent storage commodity))
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
  ([storage-spec account]
   (commodities-account-summary storage-spec account (t/today)))
  ([storage-spec account as-of]
   (with-storage [s storage-spec]
     (let [data (conj (->> (lots/search s {:account-id (:id account)})
                           (filter #(not= 0M (:shares-owned %)))
                           (group-by :commodity-id)
                           (map #(summarize-commodity s %))
                           (sort-by :caption)
                           (into []))
                      {:caption "Cash"
                       :style :data
                       :value (transactions/balance-as-of
                                s
                                account
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

(defn- append-commodity
  [{commodity-id :commodity-id :as lot} storage-spec]
  (let [{:keys [name symbol] :as commodity} (commodities/find-by-id storage-spec
                                                                    commodity-id)]
    (assoc lot
           :caption (format "%s (%s)" name symbol)
           :commodity commodity)))

(defn- append-current-price
  [lot storage-spec]
  (assoc lot :current-price (->> (:commodity lot)
                                 (prices/most-recent storage-spec)
                                 :price)))

(defn- transform-lot-transactions
  [trans]
  (map #(merge % (select-keys trans [:id :transaction-date]))
       (:lot-items trans)))

(defn- append-lot-transactions
  [lot storage-spec]
  (assoc lot
         :transactions
         (mapcat transform-lot-transactions
                 (transactions/search
                   storage-spec
                   {[:lot-transaction :lot-id] (:id lot)
                    :transaction-date [:>= (:purchase-date lot)]}
                   {:include-lot-items? true}))))

(defn- append-lot-calculated-values
  [lot]
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
   (->> (lots/search storage-spec (cond-> {:account-id account-id}
                                    commodity-id
                                    (assoc :commodity-id commodity-id)))
        (map #(-> %
                   (append-commodity storage-spec)
                   (append-current-price storage-spec)
                   (dissoc :commodity)
                   (append-lot-transactions storage-spec)
                   append-lot-calculated-values))
        (sort-by :caption)
        (map #(dissoc % :id :shares-purchased :updated-at :created-at :account-id)))))
