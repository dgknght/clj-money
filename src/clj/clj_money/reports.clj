(ns clj-money.reports
  (:require [clojure.set :refer [rename-keys
                                 intersection]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [java-time.api :as t]
            [clj-money.find-in-chunks :as ch]
            [dgknght.app-lib.inflection :refer [humanize]]
            [clj-money.util :refer [earliest]]
            [clj-money.dates :as dates]
            [clj-money.models.accounts :as accounts]
            [clj-money.accounts :refer [nest
                                        unnest
                                        left-side?]]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.lot-transactions :as lot-trans]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]))

(defn- last-item
  [inclusion date items]
  (let [pred (case inclusion
               :on-or-before #(or (= (:transaction-date %) date)
                                  (t/before? (:transaction-date %) date))
               :before #(t/before? (:transaction-date %) date)
               inclusion)]
    (when-let [filtered (->> items
                             (filter pred)
                             seq)]
      (apply max-key :index filtered))))

(defn- fetch-balances
  ([account-ids as-of] (fetch-balances account-ids as-of {}))
  ([account-ids as-of opts]
   (if (seq account-ids)
     (ch/find account-ids
              (merge
                {:start-date as-of
                 :time-step (t/years 1)
                 :fetch-fn (fn [ids date]
                             (transactions/search-items
                               {:account-id ids
                                :transaction-date [:between
                                                   (t/minus date (t/years 1))
                                                   date]}))
                 :earliest-date (t/local-date 1900 1 1) ; TODO: Get earliest date for the entity
                 :id-fn :account-id
                 :find-one-fn (partial last-item :on-or-before as-of)}
                opts))
     {})))

(defn- append-deltas
  [start end accounts]
  (let [account-ids (->> accounts
                         (map :id)
                         (into #{}))
        start-balances (fetch-balances
                        account-ids
                        start
                        {:find-one-fn (partial last-item :before start)})
        end-balances (fetch-balances account-ids end)]
    (map #(assoc % :value (- (get-in end-balances [(:id %) :balance] 0M)
                             (get-in start-balances [(:id %) :balance] 0M)))
         accounts)))

(declare summarize-accounts)
(defn- summarize-account
  [account depth]
  (let [children (->> (:children account)
                      (summarize-accounts (inc depth))
                      (sort-by :name))]
    (cons (-> account
              (select-keys [:id :name :value])
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
   (income-statement entity
                     (dates/first-day-of-the-month (t/local-date))
                     (dates/last-day-of-the-month (t/local-date))))
  ([entity start end]
   (->> (accounts/search {:entity-id (:id entity)
                          :type ["income" "expense"]})
        (append-deltas start end)
        (nest {:types [:income :expense]})
        (mapcat summarize-group)
        summarize-income-statement)))

(defn- summarize-lots
  [lots]
  (.setScale
   (->> lots
        (map :current-value)
        (reduce + 0M))
   2
   BigDecimal/ROUND_HALF_UP))

(defn- summarize-commodity-value
  [entry]
  (update-in entry [1] summarize-lots))

(defn- apply-balances
  [{:keys [balances lots] :as ctx}]
  (let [values (->> lots
                    (group-by (juxt :account-id :commodity-id))
                    (map summarize-commodity-value)
                    (into {}))]
    (update-in ctx
               [:accounts]
               (fn [accounts]
                 (map #(assoc % :value (or (get-in values [[(:parent-id %) (:commodity-id %)]])
                                           (get-in balances [(:id %) :balance])
                                           0M))
                      accounts)))))

(defn- append-balances
  [{:keys [accounts as-of] :as ctx}]
  {:pre [(:accounts ctx)]}

  (let [accounts (if (map? accounts)
                   (vals accounts)
                   accounts)]
    (assoc ctx :balances (if (seq accounts)
                           (fetch-balances (->> accounts
                                                (map :id)
                                                set)
                                           as-of)
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
(defn- calc-unrealized-gains
  [{:keys [lots]}]
  (when (seq lots)
    (.setScale
     (->> lots
          (map :gains)
          (reduce + 0M))
     2
     BigDecimal/ROUND_HALF_UP)))

(defn- append-unrealized-gains
  [mapped-accounts ctx]
  (if-let [gains (calc-unrealized-gains ctx)]
    (update-in mapped-accounts
               [:equity]
               (fnil conj [])
               {:name "Unrealized Gains"
                :value gains})
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
  [{:keys [commodity-ids as-of] :as ctx}]
  (assoc ctx :lots (if (seq commodity-ids)
                     (lots/search {:commodity-id commodity-ids
                                   :purchase-date [:<= as-of]})
                     [])))

(defmethod append-lots :implicit
  [{:keys [entity as-of] :as ctx}]
  (assoc ctx :lots (lots/search {[:commodity :entity-id] (:id entity)
                                 :purchase-date [:<= as-of]})))

(defn- fetch-lot-transactions
  [{:keys [entity as-of]}]
  (group-by :lot-id
            (lot-trans/search {[:transaction :entity-id] (:id entity)
                               :lot-action "sell"
                               :transaction-date [:<= as-of]})))

(defn- update-lot
  [lot-transactions prices {:keys [commodity-id id shares-purchased purchase-price] :as lot}]
  (let [current-price (get-in prices [commodity-id])
        current-shares (- shares-purchased
                          (->> (get-in lot-transactions [id])
                               (map :shares)
                               (reduce + 0M)))
        cost-basis (* current-shares purchase-price)
        current-value (* current-price current-shares)]
    (assoc lot
           :cost-basis cost-basis
           :current-price current-price
           :current-shares current-shares
           :current-value current-value
           :gains (- current-value cost-basis))))

(defn- update-lots
  [{:keys [lots as-of] :as ctx}]
  (let [lot-transactions (fetch-lot-transactions ctx)
        prices (if (seq lots)
                 (prices/batch-fetch (->> lots
                                          (map :commodity-id)
                                          set)
                                     {:as-of as-of
                                      :earliest-date (t/local-date 1900 1 1)}) ; TODO: get this from the entity
                 {})]
    (update-in ctx
               [:lots]
               #(map (partial update-lot lot-transactions prices)
                     %))))

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
   (balance-sheet entity (t/local-date)))
  ([entity as-of]
   (-> {:entity entity
        :as-of as-of
        :accounts (accounts/search {:entity-id (:id entity)})}
       extract-commodity-ids
       append-lots
       update-lots
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
        actual-amount (or (:value account) 0M)
        difference (if (left-side? account)
                     (- budget-amount actual-amount)
                     (- actual-amount budget-amount))]
    (with-precision 10
      (-> account
          (select-keys [:id :user-tags :parent-id])
          (merge {:caption (if (seq (:path account))
                             (string/join "/" (:path account))
                             (:name account))
                  :style :data
                  :budget budget-amount
                  :actual actual-amount
                  :difference difference
                  :percent-difference (when (not= 0M budget-amount)
                                        (with-precision 5
                                          (/ difference budget-amount)))
                  :actual-per-period (with-precision 5
                                       (/ actual-amount period-count))})))))

(defn- sum
  [attr col]
  (->> col
       (map attr)
       (reduce + 0M)))

(defn- budget-group-header
  [period-count header-key records]
  (let [[budget actual diff] (map #(sum % records)
                                  [:budget :actual :difference])
        ; If all items are grouped by Untagged, then just ignore the tag layer
        records (if (and (= 1 (count records))
                         (= "Untagged" (:caption (first records))))
                  (-> records first :items)
                  records)]
    (with-precision 10
      {:caption (humanize header-key)
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
  (->> [[:actual]
        [:budget]
        [:roll-up :actual]
        [:roll-up :budget]]
       (map #(get-in item % 0M))
       (every? zero?)))

(defn- process-tag-group
  [[tag accounts] {:keys [period-count budget] :as options}]
  (budget-group-header period-count
                       tag
                       (->> accounts
                            (map #(->budget-report-record % budget options))
                            (sort-by :difference)
                            (roll-up period-count)
                            (map #(dissoc % :parent-id))
                            (remove zero-budget-report-item?))))

(defn- group-by-tags
  [tags accounts]
  (let [tagged (->> tags
                    (map (fn [tag]
                           [tag (filter #(contains? (:user-tags %) tag) accounts)]))
                    (into {}))
        tag-set (set tags)]
    (assoc tagged
           :untagged (->> accounts
                          (filter #(= :expense (:type %))) ; TODO: consider expanding this to include any account on the budget, as one day I want to include savings, loan payments, etc.
                          (remove #(seq (intersection (:user-tags %) tag-set)))))))

(defn- any-account-has-tag?
  [tags accounts]
  (boolean
    (when (seq tags)
      (->> accounts
           (filter #(seq (intersection (set tags) (:user-tags %))))
           first))))

(defn- process-budget-group
  [[account-type accounts] {:keys [period-count tags budget] :as options}]
  (if (any-account-has-tag? (set tags) accounts)
    (let [accounts-by-tag (group-by-tags tags accounts)]
      (->> (conj tags :untagged)
           (map (comp #(with-meta % {:account-type account-type})
                      #(process-tag-group % options)
                      #(vector % (accounts-by-tag %))))
           (remove zero-budget-report-item?))) ; TODO: Is there a better place for this?
    [(with-meta
       (budget-group-header period-count
                          account-type
                          (->> accounts
                               (map #(->budget-report-record % budget options))
                               (sort-by :difference)
                               (roll-up period-count)
                               (map #(dissoc % :parent-id))
                               (remove zero-budget-report-item?)))
       {:account-type account-type})]))

(defn- summarize
  [ks records]
  (reduce (fn [result record]
            (reduce #(update-in %1 [%2] + (get-in record [%2]))
                    result
                    ks))
          (->> ks
               (map #(vector % 0M))
               (into {}))
          records))

(defn- append-summary
  [period-count records]
  (let [income (->> records
                    (filter #(= :income (-> % meta :account-type)))
                    (summarize [:budget :actual]))
        expense (->> records
                     (filter #(= :expense (-> % meta :account-type)))
                     (summarize [:budget :actual]))
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

(defn- end-of-last-month []
  (-> (t/local-date)
      (t/minus (t/months 1))
      dates/last-day-of-the-month))

(defn- default-budget-end-date
  [{:keys [start-date end-date]}]
  (->> [(end-of-last-month)
        (dates/last-day-of-the-month (t/local-date))
        end-date]
       (filter #(t/before? start-date %))
       (apply earliest)))

(defn budget
  "Returns a budget report"
  ([bdg]
   (budget bdg {}))
  ([budget {:keys [as-of] :as options}]
   (let [as-of (or as-of
                   (default-budget-end-date budget))
         context (assoc options
                        :budget budget
                        :as-of as-of
                        :period-count (if (t/after? (:end-date budget)
                                                    as-of)
                                        (inc (:index (budgets/period-containing budget as-of)))
                                        (:period-count budget)))
         items (->> (accounts/search {:entity-id (:entity-id budget)
                                      :type #{:income :expense}})
                    (append-deltas (:start-date budget) as-of)
                    (nest {:types [:income :expense]})
                    unnest
                    (group-by :type)
                    (sort-by #(.indexOf [:income :expense] (first %)))
                    (mapcat #(process-budget-group % context))
                    (append-summary (:period-count context)))]
     {:items items
      :title (format "%s: %s to %s"
                     (:name budget)
                     (t/format (t/formatter "MMMM") (:start-date budget))
                     (t/format (t/formatter "MMMM") as-of))})))

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
                                           #(dates/days-between
                                             (:start-date budget)
                                             %)))
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
   (monitor account (t/local-date)))
  ([account as-of]
   (if-let [budget (budgets/find-by-date (:entity-id account) as-of)]
     (monitor-from-budget {:account account
                           :as-of as-of
                           :budget budget})
     {:caption (:name account)
      :account account
      :message (format "There is no budget for %s" (dates/format-local-date as-of))})))

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
   (commodities-account-summary account (t/local-date)))
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
  [{:keys [current-value current-shares purchase-price] :as lot}]
  (let [cost-basis (* current-shares purchase-price)
        gain-loss (- current-value cost-basis)]
    (assoc lot
           :cost-basis cost-basis
           :gain-loss gain-loss
           :gain-loss-percent (when-not (zero? cost-basis)
                                (with-precision 2
                                  (/ gain-loss cost-basis))))))

(defn- dispatch-portfolio-fn
  [ctx]
  {:pre [(:aggregate ctx)]}
  (:aggregate ctx))

(defmulti calc-portfolio-values dispatch-portfolio-fn)

(defmethod calc-portfolio-values :by-account
  [{:keys [lots] :as ctx}]
  (assoc ctx
         :report
         (->> lots
              (map calc-gains)
              (group-by :account-id)
              (map #(update-in % [1] (fn [lots] (group-by :commodity-id lots)))))))

(defmethod calc-portfolio-values :by-commodity
  [{:keys [lots] :as ctx}]
  (assoc ctx
         :report
         (->> lots
              (map calc-gains)
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
                    :caption (dates/format-local-date (:purchase-date %))))
       (sort-by :purchase-date t/after?)
       (cons (summarize-gains {:caption (get-in commodities [commodity-id :name])
                               :id commodity-id
                               :style :subheader}
                              [:current-shares
                               :cost-basis
                               :current-value
                               :gain-loss]
                              lots))))

(defn- flatten-and-summarize-account
  [[account-id groups] {:keys [accounts balances] :as ctx}]
  (let [cash-value (or (get-in balances [account-id :balance])
                       0M)
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
  [{:keys [commodities balances] :as ctx}]
  (let [cash-value (->> (vals balances)
                        (map :balance)
                        (reduce + 0M))]
    (update-in ctx
               [:report]
               (fn [report]
                 (->> report
                      (sort-by #(get-in commodities [(first %) :name]))
                      (mapcat #(flatten-and-summarize-commodity % ctx))
                      (cons {:caption "Cash"
                             :style :subheader
                             :current-shares cash-value
                             :cost-basis cash-value
                             :current-value cash-value
                             :gain-loss 0M
                             :gain-loss-percent 0M})
                      (remove #(zero? (:current-value %)))
                      append-portfolio-summary
                      (map #(rename-keys % {:current-shares :shares-owned}))
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
                    (remove #(zero? (:current-value %)))
                    append-portfolio-summary
                    (map #(rename-keys % {:current-shares :shares-owned}))
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
  "Returns a portfolio report based on the options:
    entity-id - Identifies the entity for which a report is to be generated (required)
    as-of     - The date for which a report is to be generated. Defaults to today
    aggregate - The method, :by-commodity or :by-account, defaults to :by-commodity"
  [options]
  {:pre [(:entity options)]}

  (-> (merge {:as-of (t/local-date)
              :aggregate :by-commodity}
             options)
      append-lots
      update-lots
      append-portfolio-accounts
      append-balances
      append-commodities
      calc-portfolio-values
      flatten-and-summarize-portfolio
      :report))
