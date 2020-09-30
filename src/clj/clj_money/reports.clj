(ns clj-money.reports
  (:require [clojure.set :refer [rename-keys]]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]
            [clj-money.find-in-chunks :as ch]
            [clj-money.util :refer [format-date]]
            [clj-money.inflection :refer [humanize]]
            [clj-money.models.date-helpers :refer [available-date-range
                                                   earliest-date]]
            [clj-money.models.accounts :as accounts]
            [clj-money.accounts :refer [nest
                                        unnest
                                        left-side?]]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]))

(defn- append-deltas
  [start end accounts]
  (let [account-ids (->> accounts
                         (map :id)
                         (into #{}))
        start-balances (ch/find account-ids
                                {:start-date start
                                 :time-step (t/years 1)
                                 :fetch-fn #(transactions/search-items
                                              {:account-id %1
                                               :transaction-date [:between
                                                                  (t/minus %2 (t/years 1))
                                                                  %2]})
                                 :earliest-date (earliest-date)
                                 :id-fn :account-id
                                 :find-one-fn (fn [items]
                                                (when-let [filtered (->> items
                                                                         (filter #(t/before? (:transaction-date %) start))
                                                                         seq)]
                                                  (apply max-key :index filtered)))})
        end-balances (ch/find account-ids
                              {:start-date end
                               :time-step (t/years 1)
                               :fetch-fn (fn [ids date]
                                           (transactions/search-items
                                             {:account-id ids
                                              :transaction-date [:between
                                                                 (t/minus date (t/years 1))
                                                                 date]}))
                               :earliest-date (earliest-date)
                               :id-fn :account-id
                               :find-one-fn (fn [items]
                                              (when-let [filtered (->> items
                                                                       (filter #(or (= (:transaction-date %) end)
                                                                                    (t/before? (:transaction-date %) end)))
                                                                       seq)]
                                                (apply max-key :index filtered)))})]
    (map #(assoc % :value (- (get-in end-balances [(:id %) :balance] 0M)
                             (get-in start-balances [(:id %) :balance] 0M)))
         accounts)))

(declare summarize-accounts)
(defn- summarize-account
  [account depth]
  (let [children (summarize-accounts (inc depth) (:children account))]
      (cons (-> account
                (select-keys [:name :value])
                (rename-keys {:name :caption})
                (update-in [:value] (fn [value]
                                      (->> children
                                           (filter #(= (inc depth) (:depth %)))
                                           (map :value)
                                           (reduce + (or value 0M)))))
                (assoc :style :data
                       :depth depth))
            children)))

(defn- summarize-accounts
  ([accounts] (summarize-accounts 0 accounts))
  ([depth accounts]
   (mapcat #(summarize-account % depth) accounts)))

(defn- summarize-group
  [{:keys [type accounts]}]
  (let [children (summarize-accounts accounts)]
    (cons {:caption (humanize type)
           :type type
           :value (->> children
                       (filter #(= 0 (:depth %)))
                       (map :value)
                       (reduce +))
           :style :header}
          (vec children))))

(defn summarize-income-statement
  [records]
  (let [{:keys [income expense]} (->> records
                                      (filter :type)
                                      (map (juxt :type :value))
                                      (into {}))]
    (concat (map #(dissoc % :type) records)
            [{:caption "Net"
              :value (- income expense)
              :style :summary}])))

(defn income-statement
  "Returns the data used to populate an income statement report"
  ([entity]
   (let [[start end] (available-date-range)]
     (income-statement entity start end)))
  ([entity start end]
   (->> (accounts/search {:entity-id (:id entity)
                          :type ["income" "expense"]})
        (append-deltas start end)
        (nest {:account-types [:income :expense]})
        (mapcat summarize-group)
        summarize-income-statement)))

(defn- fetch-balances
  [accounts as-of]
  (ch/find (->> accounts
                (map :id)
                (into #{}))
           {:start-date as-of
            :time-step (t/years 1)
            :fetch-fn (fn [ids date]
                        (transactions/search-items
                          {:account-id ids
                           :transaction-date [:between
                                              (t/minus date (t/years 1))
                                              date]}))
            :earliest-date (earliest-date) ; TODO: Get earliest date for the entity
            :id-fn :account-id
            :find-one-fn (fn [items]
                           (when-let [filtered (->> items
                                                    (filter #(or (= (:transaction-date %) as-of)
                                                                 (t/before? (:transaction-date %) as-of)))
                                                    seq)]
                             (apply max-key :index filtered)))}))

(defn- apply-balance
  [{:keys [parent-id commodity-id] :as account} {:keys [lots prices balances]}]
  (if-let [lots (get-in lots [[parent-id commodity-id]])]
    (if (zero? (:quantity account))
      (assoc account :value 0M)
      (let [shares (->> lots
                        (map :shares-owned)
                        (reduce + 0M))
            price (or (get-in prices [commodity-id])
                      (:purchase-price (first lots)))]
        (when (zero? shares) (log/warnf "Lot found with zero shares for commodity-id %s" commodity-id))
        (assoc account :value (* price shares))))
    (assoc account :value (get-in balances [(:id account) :balance]))))

(defn- apply-balances
  [ctx]
  (update-in ctx [:accounts] #(map (fn [a] (apply-balance a ctx)) %)))

(defn- append-balances
  [{:keys [accounts as-of] :as ctx}]
  (assoc ctx :balances (fetch-balances accounts as-of)))

(defn- fetch-prices
  ([commodity-ids] (fetch-prices commodity-ids (t/today)))
  ([commodity-ids as-of]
   (ch/find commodity-ids {:start-date as-of
                           :time-step (t/years 1)
                           :fetch-fn #(prices/search
                                        {:commodity-id %1
                                         :trade-date [:between
                                                      (t/minus %2 (t/years 1))
                                                      %2]})
                           :transform-fn :price
                           :id-fn :commodity-id
                           :earliest-date (earliest-date)
                           :find-one-fn (fn [prices]
                                          (apply max-key
                                                 (comp tc/to-long :trade-date)
                                                 (filter #(or (= as-of (:trade-date %))
                                                              (t/before? (:trade-date %) as-of))
                                                         prices)))})))

(defn- append-prices
  [{:keys [lots as-of] :as ctx}]
  (let [commodity-ids (->> (if (map? lots)
                             (flatten (vals lots))
                             lots)
                           (map :commodity-id)
                           set)]
    (assoc ctx :prices (if (seq commodity-ids)
                         (fetch-prices commodity-ids as-of)
                         {}))))

(defn- append-retained-earnings
  [mapped-accounts]
  (let [{:keys [income expense]} (->> (select-keys mapped-accounts [:income :expense])
                                      (map #(hash-map :type (first %) :accounts (second %)))
                                      (mapcat summarize-group)
                                      (filter :type)
                                      (map (juxt :type :value))
                                      (into {}))]
    (update-in mapped-accounts [:equity] (fnil conj []) {:name "Retained Earnings"
                                                         :value (- income expense)})))
(defn- append-unrealized-gains
  [mapped-accounts {:keys [lots prices]}]
  (if (seq lots)
    (update-in mapped-accounts
               [:equity]
               (fnil conj [])
               {:name "Unrealized Gains"
                :value (->> (flatten (vals lots))
                            (map #(assoc % :current-price (get-in prices [(:commodity-id %)])))
                            (map (fn [{:keys [current-price purchase-price shares-owned]}]
                                   (if current-price
                                     (* shares-owned
                                        (- current-price
                                           purchase-price))
                                     0M)))
                            (reduce + 0M))})
    mapped-accounts))

(defn- summarize-balance-sheet
  [{:keys [accounts] :as ctx}]
  (let [mapped (->> accounts
                    nest
                    (map (juxt :type :accounts))
                    (into {}))
        with-pseudo (-> mapped
                        (append-retained-earnings)
                        (append-unrealized-gains ctx))
        records (->> [:asset :liability :equity]
                     (map #(hash-map :type % :accounts (% with-pseudo)))
                     (mapcat summarize-group))
        totals (->> records
                    (filter :type)
                    (map (juxt :type :value))
                    (into {}))]
    (concat (map #(dissoc % :type) records)
            [{:caption "Liabilities + Equity"
              :value (+ (get-in totals [:liability])
                        (get-in totals [:equity]))
              :style :summary}])))

(defn- extract-commodity-ids
  [{:keys [accounts entity] :as ctx}]
  (assoc ctx :commodity-ids (->> accounts
                                 (map :commodity-id)
                                 (remove #(= % (get-in entity [:settings :default-commodity-id])))
                                 (into #{}))))

(defmulti ^:private append-lots
  (fn [{:keys [commodity-ids]}]
    (if (nil? commodity-ids)
      :implicit
      :explicit)))

(defmethod append-lots :explicit
  [{:keys [commodity-ids] :as ctx}]
  (assoc ctx :lots (if (seq commodity-ids)
                     (->> (lots/search {:commodity-id commodity-ids
                                        :shares-owned [:!= 0]})
                          (group-by (juxt :account-id :commodity-id))
                          (into {}))
                     {})))

(defmethod append-lots :implicit
  [{:keys [entity-id] :as ctx}]
  (assoc ctx :lots (lots/search {[:commodity :entity-id] entity-id
                                 :shares-owned [:!= 0M] })))

(defn- check-balance
  [report]
  (let [balances (->> report
                      (filter #(= :header (:style %)))
                      (map (juxt :caption :value))
                      (into {}))]
    (when-not (= (balances "Asset")
                 (+ (balances "Liability")
                    (balances "Equity")))
      (log/warnf "Balance sheet out of balance. Asset: %s, Liability + Equity %s, difference %s"
                 (balances "Asset")
                 (+ (balances "Liability")
                    (balances "Equity"))
                 (- (balances "Asset")
                    (+ (balances "Liability")
                       (balances "Equity"))))))
  report)

(defn balance-sheet
  "Returns the data used to populate a balance sheet report"
  ([entity]
   (balance-sheet entity (t/today)))
  ([entity as-of]
   (-> {:entity entity
        :as-of as-of
        :accounts (accounts/search {:entity-id (:id entity)})}
       extract-commodity-ids
       append-lots
       append-prices
       append-balances
       apply-balances
       summarize-balance-sheet
       check-balance)))

(defn- ->budget-report-record
  [account budget {:keys [period-count]}]
  (let [item (budgets/find-item-by-account budget account)
        budget-amount (if item
                        (reduce + 0M (->> item
                                          :periods
                                          (take period-count)))
                        0M) ; TODO only total the periods up to and including the as-of date
        actual-amount (:value account)
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
  [budget {:keys [period-count] :as options} [account-type accounts]]
  (budget-group-header period-count
                       account-type
                       (->> accounts
                            (map #(->budget-report-record % budget options))
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
  ([bdg]
   (budget bdg {}))
  ([budget {:keys [as-of] :as options}]
   (let [as-of (or as-of
                   (default-budget-end-date budget))
         context (assoc options
                        :as-of as-of
                        :period-count (if (t/after? (:end-date budget)
                                                    as-of)
                                        (inc (:index (budgets/period-containing budget as-of)))
                                        (:period-count budget)))
         items (->> (accounts/search {:entity-id (:entity-id budget)
                                      :type #{:income :expense}})
                    (append-deltas (:start-date budget) as-of)
                    nest
                    unnest
                    (group-by :type)
                    (sort-by  #(.indexOf [:income :expense] (first %)))
                    (map #(process-budget-group budget context %))
                    (append-summary (:period-count context)))]
     {:items items
      :title (format "%s: %s to %s"
                     (:name budget)
                     (tf/unparse-local-date (tf/formatter "MMMM") (:start-date budget))
                     (tf/unparse-local-date (tf/formatter "MMMM") as-of))})))

(defn- monitor-item
  [budget actual percentage]
  {:total-budget budget
   :actual actual
   :percentage percentage
   :prorated-budget (* percentage budget)
   :actual-percent (/ actual budget)})

(defn- aggregate-account-actuals
  [accounts start end]
  (->> accounts
       (map #(transactions/balance-delta % start end))
       (reduce + 0M)))

(defn- monitor-from-item
  [{:keys [account as-of budget children] {:keys [periods]} :item}]
  (let [period (budgets/period-containing budget as-of)
        period-budget (nth periods (:index period) 0M)
        total-budget (reduce + 0M periods)
        percent-of-period (budgets/percent-of-period budget
                                                     as-of)
        period-actual (aggregate-account-actuals (conj children account)
                                                 (:start period)
                                                 as-of)
        total-actual (aggregate-account-actuals (conj children account)
                                                (:start-date budget)
                                                as-of)
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
  [{:keys [budget account]}]
  (let [items (budgets/find-items-by-account budget account)]
    (when (seq items)
      {:account account
       :periods (->> items
                     (map :periods)
                     (apply interleave)
                     (partition (count items))
                     (map #(reduce + %)))})))

(defn- append-account-children
  [{:keys [account] :as ctx}]
  (let [children (remove
                   #(= (:id %)  (:id account))
                   (accounts/search {:id (:id account)}
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
  ([account]
   (monitor account (t/today)))
  ([account as-of]
   (if-let [budget (budgets/find-by-date (:entity-id account) as-of)]
     (monitor-from-budget {:account account
                           :as-of as-of
                           :budget budget})
     {:caption (:name account)
      :account account
      :message (format "There is no budget for %s" (format-date as-of))})))

(defn- summarize-commodity
  [[commodity-id lots]]
  (let [commodity (commodities/find commodity-id)
        shares (sum :shares-owned lots)
        cost (->> lots
                  (map #(* (:shares-owned %)
                           (:purchase-price %)))
                  (reduce +))
        price (:price (prices/most-recent commodity))
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
  ([account]
   (commodities-account-summary account (t/today)))
  ([account as-of]
   (let [data (conj (->> (lots/search {:account-id (:id account)})
                         (filter #(not= 0M (:shares-owned %)))
                         (group-by :commodity-id)
                         (map #(summarize-commodity %))
                         (sort-by :caption)
                         (into []))
                    {:caption "Cash"
                     :style :data
                     :value (transactions/balance-as-of
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
     (conj data summary))))

(defn- append-commodity
  [{:keys [commodity-id] :as lot}]
  (let [{:keys [name symbol] :as commodity} (commodities/find commodity-id)]
    (assoc lot
           :caption (format "%s (%s)" name symbol)
           :commodity commodity)))

(defn- append-current-price
  [lot]
  (assoc lot :current-price (->> (:commodity lot)
                                 prices/most-recent
                                 :price)))

(defn- transform-lot-transactions
  [trans]
  (map #(merge % (select-keys trans [:id :transaction-date]))
       (:lot-items trans)))

(defn- append-lot-transactions
  [lot]
  (assoc lot
         :transactions
         (mapcat transform-lot-transactions
                 (transactions/search
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
  ([account-id]
   (lot-report account-id nil))
  ([account-id commodity-id]
   (->> (lots/search (cond-> {:account-id account-id}
                       commodity-id
                       (assoc :commodity-id commodity-id)))
        (map #(-> %
                   append-commodity
                   append-current-price
                   (dissoc :commodity)
                   append-lot-transactions
                   append-lot-calculated-values))
        (sort-by :caption)
        (map #(dissoc % :id :shares-purchased :updated-at :created-at :account-id)))))

(defn- append-portfolio-accounts
  [{:keys [lots] :as ctx}]
  (assoc ctx :accounts (if (seq lots)
                         (->> (accounts/search {:id (set (map :account-id lots))})
                              (map (juxt :id identity))
                              (into {}))
                         {})))

(defn- append-commodities
  [{:keys [lots] :as ctx}]
  (assoc ctx :commodities (if (seq lots)
                            (->> (commodities/search
                                   {:id (->> lots
                                             (map :commodity-id)
                                             set)})
                                 (map (juxt :id identity))
                                 (into {}))
                            {})))

(defn- calc-gains
  [{:keys [commodity-id shares-owned purchase-price] :as lot}
   {:keys [prices]}]
  (let [current-price (get-in prices [commodity-id])
        cost-basis (* shares-owned purchase-price)
        current-value (* shares-owned current-price)
        gain-loss (- current-value cost-basis)]
    (assoc lot
           :current-price current-price
           :cost-basis cost-basis
           :current-value current-value
           :gain-loss gain-loss
           :gain-loss-percent (when-not (zero? cost-basis)
                                (with-precision 2
                                  (/ gain-loss cost-basis))))))

(defn- dispatch-portfolio-fn
  [{:keys [options]}]
  {:pre [(:aggregate options)]}
  (:aggregate options))

(defmulti calc-portfolio-values dispatch-portfolio-fn)

(defmethod calc-portfolio-values :by-account
  [{:keys [lots] :as ctx}]
  (assoc ctx
         :report
         (->> lots
              (map #(calc-gains % ctx))
              (group-by :account-id)
              (map #(update-in % [1] (fn [lots] (group-by :commodity-id lots)))))))

(defmethod calc-portfolio-values :by-commodity
  [{:keys [lots] :as ctx}]
  (assoc ctx
         :report
         (->> lots
              (map #(calc-gains % ctx))
              (group-by :commodity-id))))

(defn- sum-fields
  [target fields coll]
  (reduce #(assoc %1 %2 (sum %2 coll))
           target
           fields))

(defn- calc-gain-loss-percent
  [{:keys [cost-basis gain-loss] :as target}]
  (assoc target :gain-loss-percent (when-not (zero? cost-basis)
                                     (with-precision 2
                                       (/ gain-loss cost-basis)))))

(defn- summarize-gains
  ([target coll]
   (summarize-gains target
                    [:cost-basis
                     :current-value
                     :gain-loss]
                    coll))
  ([target fields coll]
   (-> target
       (sum-fields fields coll)
       calc-gain-loss-percent)))

(defn- flatten-and-summarize-commodity
  [[commodity-id lots] {:keys [commodities]}]
  (->> lots
       (map #(assoc %
                    :parents #{commodity-id}
                    :style :data
                    :caption (format-date (:purchase-date %))))
       (sort-by :purchase-date t/after?)
       (cons (summarize-gains {:caption (get-in commodities [commodity-id :name])
                               :id commodity-id
                               :style :subheader}
                              [:shares-owned
                               :cost-basis
                               :current-value
                               :gain-loss]
                              lots))))

(defn- flatten-and-summarize-account
  [[account-id groups] {:keys [accounts] :as ctx}]
  (let [cash-value (get-in accounts [account-id :quantity])
        children (cons
                   {:caption "Cash"
                    :style :subheader
                    :current-value cash-value
                    :cost-basis cash-value
                    :gain-loss 0M}
                   (mapcat #(flatten-and-summarize-commodity % ctx)
                           groups))]
    (->> children
         (map #(update-in % [:parents] (fnil conj #{}) account-id))
         (cons (summarize-gains {:caption (get-in accounts [account-id :name])
                                 :style :header
                                 :id account-id}
                                (filter #(= :subheader (:style %)) children))))))

(defn- append-portfolio-summary
  [records]
  (concat records
          [(summarize-gains
             {:caption "Total"
              :style :summary
              :id :summary}
             (remove :parents records))]))

(defmulti flatten-and-summarize-portfolio dispatch-portfolio-fn)

(defmethod flatten-and-summarize-portfolio :by-commodity
  [{:keys [commodities accounts] :as ctx}]
  (let [cash-value (->> (vals accounts)
                        (map :quantity)
                        (reduce + 0M))]
    (update-in ctx
               [:report]
               (fn [report]
                 (->> report
                      (sort-by #(get-in commodities [(first %) :name]))
                      (mapcat #(flatten-and-summarize-commodity % ctx))
                      (cons {:caption "Cash"
                             :style :subheader
                             :shares-owned cash-value
                             :cost-basis cash-value
                             :current-value cash-value
                             :gain-loss 0M
                             :gain-loss-percent 0M})
                      append-portfolio-summary
                      (map #(select-keys % [:caption
                                            :style
                                            :shares-purchased
                                            :shares-owned
                                            :cost-basis
                                            :current-value
                                            :gain-loss
                                            :gain-loss-percent
                                            :id
                                            :parents])))))))

(defmethod flatten-and-summarize-portfolio :by-account
  [{:keys [accounts] :as ctx}]
  (update-in ctx
             [:report]
             (fn [report]
               (->> report
                    (sort-by #(get-in accounts [(first %) :name]))
                    (mapcat #(flatten-and-summarize-account % ctx))
                    append-portfolio-summary
                    (map #(select-keys % [:caption
                                          :style
                                          :shares-purchased
                                          :shares-owned
                                          :cost-basis
                                          :current-value
                                          :gain-loss
                                          :gain-loss-percent
                                          :id
                                          :parents]))))))

(defn portfolio
  ([entity-id] (portfolio entity-id {}))
  ([entity-id options]
   (-> {:entity-id entity-id
        :as-of (t/today) ; TODO: add support for historical portfolio report
        :options (merge {:aggregate :by-commodity} options)}
       append-lots
       append-portfolio-accounts
       append-commodities
       append-prices
       calc-portfolio-values
       flatten-and-summarize-portfolio
       :report)))
