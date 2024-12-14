(ns clj-money.reports
  (:require [clojure.set :refer [rename-keys
                                 intersection]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [java-time.api :as t]
            [clj-money.find-in-chunks :as ch]
            [clj-money.models :as models]
            [dgknght.app-lib.inflection :refer [humanize]]
            [clj-money.util :as util :refer [earliest
                                             model=]]
            [clj-money.dates :as dates]
            [clj-money.accounts :refer [nest
                                        unnest
                                        left-side?]]
            [clj-money.db :as db]
            [clj-money.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.prices :as prices]
            [clj-money.models.budgets :as bdgs]))

(defn- header?
  [{:report/keys [style]}]
  (= :header style))

(defn- equity?
  [{:report/keys [type]}]
  (= :equity type))

(def ^:private equity-header?
  (every-pred header? equity?))

(defn- max-by-item-index
  [items]
  (when (seq items)
    (apply max-key :transaction-item/index items)))

(defn- last-item
  [inclusion date items]
  (let [pred (case inclusion
               :on-or-before (fn [{:transaction-item/keys [transaction-date]}]
                               (or (= transaction-date date)
                                   (t/before? transaction-date date)))
               :before #(t/before? (:transaction-item/transaction-date %) date)
               inclusion)]
    (->> items
         (filter pred)
         max-by-item-index)))

(defn- fetch-balances
  [account-ids as-of & {:as opts}]
  (if (seq account-ids)
    (ch/find account-ids
             (merge
               {:start-date as-of
                :time-step (t/years 1)
                :fetch-fn (fn [ids date]
                            (models/select
                              #:transaction-item{:account [:in ids]
                                                 :transaction-date [:<between
                                                                    (t/minus date (t/years 1))
                                                                    date]}))
                :id-fn (comp :id :transaction-item/account)
                :find-one-fn (partial last-item :on-or-before as-of)}
               opts))
    {}))

(defn- append-deltas
  [{:keys [start end earliest-date]} accounts]
  (let [account-ids (->> accounts
                         (map :id)
                         set)
        start-balances (fetch-balances
                         account-ids
                         start
                         :find-one-fn (partial last-item :before start)
                         :earliest-date earliest-date)
        end-balances (fetch-balances
                       account-ids
                       end
                       :earliest-date earliest-date)]
    (map (fn [a]
           (assoc a :account/value (- (get-in end-balances [(:id a) :transaction-item/balance] 0M)
                                      (get-in start-balances [(:id a) :transaction-item/balance] 0M))))
         accounts)))

(declare summarize-accounts)
(defn- summarize-account
  [account depth]
  (let [children (->> (:account/children account)
                      (summarize-accounts (inc depth))
                      (sort-by :account/name))]
    (cons (-> account
              (select-keys [:id
                            :account/name
                            :account/total-value
                            :account/type])
              (rename-keys {:account/name :report/caption
                            :account/total-value :report/value
                            :account/type :report/type})
              (assoc :report/style :data
                     :report/depth depth))
          children)))

(defn- summarize-accounts
  ([accounts] (summarize-accounts 0 accounts))
  ([depth accounts]
   (mapcat #(summarize-account % depth) accounts)))

(defn- summarize-account-type
  [{:keys [type accounts]}]
  (let [records (summarize-accounts accounts)]
    (cons {:report/caption (humanize type)
           :report/type type
           :report/style :header
           :report/value (->> records
                              (filter #(-> % :report/depth zero?))
                              (map :report/value)
                              (reduce + 0M))}
          (vec records))))

(defn- map-record-headers
  [records]
  (->> records
       (filter header?)
       (map (juxt :report/type :report/value))
       (into {})))

(defn summarize-income-statement
  [records]
  (let [{:keys [income expense]} (map-record-headers records)]
    (concat (map #(dissoc % :report/type) records)
            [{:report/caption "Net"
              :report/value (- income expense)
              :report/style :summary}])))

(defn income-statement
  "Returns the data used to populate an income statement report"
  ([entity]
   (income-statement entity
                     (dates/first-day-of-the-month (t/local-date))
                     (dates/last-day-of-the-month (t/local-date))))
  ([{:as entity :entity/keys [settings]} start end]
   (->> (models/select #:account{:entity entity
                                 :type [:in #{:income :expense}]})
        (append-deltas {:start start
                        :end end
                        :earliest-date (:settings/earliest-transaction-date settings)})
        (nest {:types [:income :expense]})
        (mapcat summarize-account-type)
        summarize-income-statement)))

(defn- summarize-lots
  [lots]
  (.setScale
   (->> lots
        (map :lot/current-value)
        (reduce + 0M))
   2
   BigDecimal/ROUND_HALF_UP))

(defn- summarize-commodity-value
  [entry]
  (update-in entry [1] summarize-lots))

(defn- apply-balances
  [{:keys [balances lots] :as ctx}]
  (let [values (->> lots
                    (group-by (juxt (comp :id :lot/account)
                                    (comp :id :lot/commodity)))
                    (map summarize-commodity-value)
                    (into {}))]
    (update-in ctx
               [:accounts]
               (fn [accounts]
                 (map #(assoc %
                              :account/value
                              (or (get-in values [[(:id (:account/parent %))
                                                   (:id (:account/commodity %))]])
                                  (get-in balances [(:id %) :transaction-item/balance])
                                  0M))
                      accounts)))))

(defn- append-balances
  [{:keys [accounts as-of entity] :as ctx}]
  {:pre [(:accounts ctx)]}

  (let [accounts (if (map? accounts)
                   (vals accounts)
                   accounts)
        balances (if (seq accounts)
                   (fetch-balances (->> accounts
                                        (map :id)
                                        set)
                                   as-of
                                   :earliest-date (get-in entity [:entity/settings
                                                                  :settings/earliest-transaction-date]))
                   {})]
    (assoc ctx :balances balances)))

(defn- append-records
  [{:keys [accounts] :as ctx}]
  (let [mapped (->> accounts
                    nest
                    (map (juxt :type :accounts))
                    (into {}))]
    (assoc ctx :records (->> [:asset :liability :equity :income :expense]
                             (map #(hash-map :type % :accounts (% mapped)))
                             (mapcat summarize-account-type)
                             vec))))

(defn- extract-commodity-refs
  [{:keys [accounts entity] :as ctx}]
  (let [default (get-in entity [:entity/settings
                                :settings/default-commodity])
        commodities (->> accounts
                         (map :account/commodity)
                         (remove #(model= % default))
                         (into #{}))]
    (assoc ctx :commodities commodities)))

(defn- append-lots
  [{:keys [commodities as-of entity] :as ctx}]
  (assoc ctx
         :lots
         (if (seq commodities)
           (models/select #:lot{:commodity [:in (map :id commodities)]
                                :purchase-date [:<= as-of]})
           (models/select (db/model-type
                            {:commodity/entity entity
                             :lot/purchase-date [:<= as-of]}
                            :lot)))))

(defn- fetch-lot-items
  [{:keys [entity as-of]}]
  (group-by (comp :id :lot-item/lot)
            (models/select (db/model-type
                             {:transaction/entity entity
                              :lot-item/lot-action :sell
                              :lot-item/transaction-date [:<= as-of]}
                             :lot-item))))

(defn- append-lot-items
  [ctx]
  (assoc ctx
         :lot-items
         (fetch-lot-items ctx)))

(defn- realize-commodity-refs
  [ctx]
  (update-in ctx
             [:commodities]
             #(models/select (db/model-type
                               {:id [:in (mapv :id %)]}
                               :commodity))))

(defn- append-latest-prices
  [{:as ctx :keys [commodities as-of]}]
  (assoc ctx
         :prices
         (->> commodities
              (map #(prices/most-recent % as-of))
              (map (juxt (comp :id :price/commodity)
                         :price/price))
              (into {}))))

(defn- update-lot
  [{:lot/keys [commodity
               shares-purchased
               purchase-price]
    :keys [id]
    :as lot}
   {:keys [prices
           lot-items]}]
  (let [current-price (get-in prices [(:id commodity)])
        current-shares (- shares-purchased
                          (->> (get-in lot-items [id])
                               (map :lot-item/shares)
                               (reduce + 0M)))
        cost-basis (* current-shares purchase-price)
        current-value (* current-price current-shares)]
    (assoc lot
           :lot/cost-basis cost-basis
           :lot/current-price current-price
           :lot/current-shares current-shares
           :lot/current-value current-value
           :lot/gains (- current-value cost-basis))))

(defn- update-lots
  "Append current price and value information to the lots"
  [ctx]
  (update-in ctx
             [:lots]
             (fn [lots]
               (map #(update-lot % ctx)
                    lots))))

(defn- update-equity-total
  [records delta]
  (mapv (fn [r]
          (if (equity-header? r)
            (update-in r [:report/value] + delta)
            r))
        records))

(defn- append-equity-record
  [ctx calc-fn caption]
  (let [value (calc-fn ctx)]
    (if (not (zero? value))
      (update-in ctx [:records] #(-> %
                                     (conj {:report/caption caption
                                            :report/style :data
                                            :report/type :equity
                                            :report/depth 0
                                            :report/value value})
                                     (update-equity-total value)))
      ctx)))

(defn- calc-retained-earnings
  [{:keys [records]}]
  (let [{:keys [income expense]} (map-record-headers records)]
    (- income expense)))

(defn- append-retained-earnings
  [ctx]
  (append-equity-record
    ctx
    calc-retained-earnings
    "Retained Earnings"))

(defn- calc-unrealized-gains
  [{:keys [lots]}]
  (if (seq lots)
    (.setScale
      (->> lots
           (map :lot/gains)
           (reduce + 0M))
      2
      BigDecimal/ROUND_HALF_UP)
    0M))

(defn- append-unrealized-gains
  [ctx]
  (append-equity-record
    ctx
    calc-unrealized-gains
    "Unrealized Gains"))

(defn- calc-liabilities-plus-equity
  [{:keys [records]}]
  (let [{:keys [liability equity]} (map-record-headers records)]
    (+ liability equity )))

(defn- append-balance-sheet-summary
  [ctx]
  (update-in ctx [:records] conj {:report/caption "Liabilities + Equity"
                                  :report/type :equity
                                  :report/style :summary
                                  :report/value (calc-liabilities-plus-equity ctx)}))

(defn- extract-balance-sheet
  [{:keys [records]}]
  (filter #(#{:asset :liability :equity} (:report/type %))
          records))

(defn- check-balance
  [records]
  (let [{:keys [asset liability equity]} (map-record-headers records)
        l-and-e (+ liability equity)]
    (when-not (= asset
                 l-and-e)
      (log/warnf "Balance sheet out of balance. Asset: %s, Liability + Equity %s, difference %s"
                 asset
                 l-and-e
                 (- asset l-and-e))))
  records)

(defn balance-sheet
  "Returns the data used to populate a balance sheet report"
  ([entity]
   (balance-sheet entity (t/local-date)))
  ([entity as-of]
   (-> {:entity entity
        :as-of as-of
        :accounts (models/select {:account/entity entity})}
       extract-commodity-refs
       realize-commodity-refs
       append-latest-prices
       append-lots
       append-lot-items
       update-lots
       append-balances
       apply-balances
       append-records
       append-retained-earnings
       append-unrealized-gains
       append-balance-sheet-summary
       extract-balance-sheet
       check-balance)))

(defn- ->budget-report-record
  [account budget {:keys [period-count]}]
  (let [item (budgets/find-item-by-account budget account)
        budget-amount (if item
                        (reduce + 0M (->> item
                                          :budget-item/periods
                                          (take period-count)))
                        0M) ; TODO only total the periods up to and including the as-of date
        actual-amount (or (:account/value account) 0M)
        difference (if (left-side? account)
                     (- budget-amount actual-amount)
                     (- actual-amount budget-amount))]
    (with-precision 10
      (-> account
          (select-keys [:id
                        :account/user-tags
                        :account/parent])
          (merge #:report{:caption (if (seq (:account/path account))
                                     (string/join "/" (:account/path account))
                                     (:account/name account))
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
                                  [:report/budget
                                   :report/actual
                                   :report/difference])
        ; If all items are grouped by Untagged, then just ignore the tag layer
        records (if (and (= 1 (count records))
                         (= "Untagged" (:report/caption (first records))))
                  (-> records first :report/items)
                  records)]
    (with-precision 10
      #:report{:caption (humanize header-key)
               :style :header
               :budget budget
               :actual actual
               :difference diff
               :percent-difference (when-not (zero? budget)
                                     (/ diff budget))
               :actual-per-period  (/ actual period-count)
               :items records})))

(defn- append-roll-up
  [{:report/keys [budget actual difference] :as record} children period-count]
  (let [b (+ budget (sum :report/budget children))
        a (+ actual (sum :report/actual children))
        d (+ difference (sum :report/difference children))]
    (assoc record
           :report/roll-up #:report{:budget b
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
        r (assoc record :report/depth depth)]
    (if (seq children)
      (conj children (append-roll-up r children period-count))
      [r])))

(defn- roll-up
  [period-count records]
  (let [records-map (reduce (fn [m r]
                              (update-in m [(:id (:account/parent r))] (fnil conj []) r))
                            {}
                            records)]
    (->> records
         (remove :account/parent)
         (mapcat #(roll-up* % 0 records-map period-count)))))

(defn- zero-budget-report-item?
  [item]
  (->> [[:report/actual]
        [:report/budget]
        [:report/roll-up :report/actual]
        [:report/roll-up :report/budget]]
       (map #(get-in item % 0M))
       (every? zero?)))

(defn- process-tag-group
  [[tag accounts] {:keys [period-count budget] :as options}]
  (budget-group-header period-count
                       tag
                       (->> accounts
                            (map #(->budget-report-record % budget options))
                            (sort-by :report/difference)
                            (roll-up period-count)
                            (map #(dissoc % :account/parent))
                            (remove zero-budget-report-item?))))

(defn- group-by-tags
  [tags accounts]
  (let [tagged (->> tags
                    (map (fn [tag]
                           [tag (filter #(contains? (:account/user-tags %) tag) accounts)]))
                    (into {}))
        tag-set (set tags)]
    (assoc tagged
           :untagged (->> accounts
                          (filter #(= :expense (:account/type %))) ; TODO: consider expanding this to include any account on the budget, as one day I want to include savings, loan payments, etc.
                          (remove #(seq (intersection (:account/user-tags %) tag-set)))))))

(defn- any-account-has-any-tag?
  [tags accounts]
  (boolean
    (when (seq tags)
      (some #(seq (intersection (set tags)
                                (:account/user-tags %)))
            accounts))))

(defn- process-budget-group
  [[account-type accounts] {:keys [period-count tags budget] :as options}]
  (if (any-account-has-any-tag? (set tags) accounts)
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
                               (sort-by :report/difference)
                               (roll-up period-count)
                               (map #(dissoc % :account/parent))
                               (remove zero-budget-report-item?)))
       {:account-type account-type})]))

(defn- summarize
  [ks records]
  (reduce (fn [result record]
            (reduce (fn [acc k]
                      (update-in acc
                                 [k]
                                 (fnil + 0M)
                                 (get-in record [k] 0M)))
                    result
                    ks))
          {}
          records))

(defn- append-budget-summary
  [{:keys [period-count]} records]
  (let [income (->> records
                    (filter #(= :income (-> % meta :account-type)))
                    (summarize [:report/budget
                                :report/actual]))
        expense (->> records
                     (filter #(= :expense (-> % meta :account-type)))
                     (summarize [:report/budget
                                 :report/actual]))
        budget (if (and income expense)
                 (->> [income expense]
                      (map #(:report/budget %))
                      (apply -))
                 0M)
        actual (if (and income expense)
                 (->> [income expense]
                      (map #(:report/actual %))
                      (apply -))
                 0M)
        difference (- actual budget)]
    (with-precision 10
      (concat records [#:report{:caption "Net"
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

(defn- append-entity
  [{:keys [budget] :as ctx}]
  (assoc ctx :entity (models/find (:budget/entity budget)
                                  :entity)))

(defn- append-period-count
  [{:keys [budget as-of] :as ctx}]
  (assoc ctx
         :period-count
         (if (t/after? (:budget/end-date budget)
                       as-of)
           (inc (:index (budgets/period-containing budget as-of)))
           (:budget/period-count budget))))

(defn- calc-budget-records
  [{:keys [budget as-of entity] :as ctx}]
  (->> (models/select #:account{:entity (:budget/entity budget)
                                :type [:in [:income :expense]]})
       (append-deltas
           {:start (:budget/start-date budget)
            :end as-of
            :earliest-date (get-in entity [:entity/settings
                                           :settings/earliest-transaction-date])})
       (nest {:types [:income :expense]})
       unnest
       (group-by :account/type)
       (mapcat #(process-budget-group % ctx))
       (append-budget-summary ctx)))

(defn budget
  "Returns a budget report"
  ([bdg]
   (budget bdg {}))
  ([budget {:keys [as-of] :as opts}]
   {:items (-> opts
               (merge {:budget budget
                       :as-of (or as-of
                                  (default-budget-end-date budget))})
               append-entity
               append-period-count
               calc-budget-records)
    :title (format "%s: %s to %s"
                   (:budget/name budget)
                   (t/format (t/formatter "MMMM") (:budget/start-date budget))
                   (t/format (t/formatter "MMMM") as-of))}))

(defn- monitor-item
  [budget actual percentage]
  {:pre [(not (zero? budget))]}

  #:report{:total-budget budget
           :actual actual
           :percentage percentage
           :prorated-budget (* percentage budget)
           :actual-percent (/ actual budget)})

(defn- aggregate-account-actuals
  [accounts start end]
  {:pre [start end (t/before? start end)]}
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
                                                (:budget/start-date budget)
                                                as-of)
        percent-of-total (with-precision 5
                           (->> [as-of (:budget/end-date budget)]
                                (map (comp inc
                                           #(dates/days-between
                                             (:budget/start-date budget)
                                             %)))
                                (apply /)))]
    (with-precision 5
      #:report{:caption (:account/name account)
               :account account
               :period (monitor-item period-budget period-actual percent-of-period)
               :budget (monitor-item total-budget total-actual percent-of-total)})))

(defn- aggregate-item
  [{:keys [budget account]}]
  (let [items (bdgs/find-items-by-account budget account)]
    (when (seq items)
      {:account account
       :periods (->> items
                     (map :budget-item/periods)
                     (apply interleave)
                     (partition (count items))
                     (mapv #(reduce + %)))})))

(defn- append-account-children
  [{:keys [account] :as ctx}]
  (let [children (remove
                  #(model= %  account)
                  (models/select (db/model-type
                                   (util/->model-ref account)
                                   :account)
                                 {:include-children? true}))]
    (-> ctx
        (update-in [:account] assoc :account/child-ids (map :id children))
        (assoc :children children))))

(defn- monitor-from-budget
  [{:keys [account] :as ctx}]
  (if-let [item (aggregate-item ctx)]
    (monitor-from-item (-> ctx
                           (assoc :item item)
                           append-account-children))
    #:report{:caption (:account/name account)
             :account account
             :message "There is no budget item for this account"}))

(defn monitor
  "Returns a mini-report for a specified account against a budget period"
  ([account]
   (monitor account (t/local-date)))
  ([account as-of]
   {:pre [account as-of]}

   (if-let [budget (bdgs/find-by-date (:account/entity account) as-of)]
     (monitor-from-budget {:account account
                           :as-of as-of
                           :budget budget})
     #:report{:caption (:account/name account)
              :account account
              :message (format "There is no budget for %s" (dates/format-local-date as-of))})))

(defn- summarize-commodity
  [[commodity-id lots]]
  (let [commodity (models/find commodity-id :commodity)
        shares (sum :lot/shares-owned lots)
        cost (->> lots
                  (map #(* (:lot/shares-owned %)
                           (:lot/purchase-price %)))
                  (reduce +))
        price (:price/price (prices/most-recent commodity))
        value (* price shares)
        gain (- value cost)]
    #:report{:caption (format "%s (%s)"
                              (:commodity/name commodity)
                              (:commodity/symbol commodity))
             :commodity (util/->model-ref commodity)
             :style :data
             :depth 0
             :shares shares
             :price price
             :cost cost
             :value value
             :gain gain}))

(defn commodities-account-summary
  ([account]
   (commodities-account-summary account (t/local-date)))
  ([account as-of]
   (let [records (conj (->> (models/select {:lot/account account
                                            :lot/shares-owned [:!= 0]})
                            (group-by (comp :id :lot/commodity))
                            (map #(summarize-commodity %))
                            (sort-by :report/caption)
                            (into []))
                       #:report{:caption "Cash"
                                :style :data
                                :depth 0
                                :value (transactions/balance-as-of
                                         account
                                         as-of)})
         summary (reduce (fn [result record]
                           (reduce (fn [r k]
                                     (update-in r [k] #(+ % (or (k record) 0M))))
                                   result
                                   [:report/cost
                                    :report/value
                                    :report/gain]))
                         #:report{:caption "Total"
                                  :style :summary
                                  :cost 0M
                                  :value 0M
                                  :gain 0M}
                         records)]
     (conj records summary))))

; (defn- append-commodity
;   [{:keys [commodity-id] :as lot}]
;   (let [{:keys [name symbol] :as commodity} (commodities/find commodity-id)]
;     (assoc lot
;            :caption (format "%s (%s)" name symbol)
;            :commodity commodity)))
;
; (defn- append-current-price
;   [lot]
;   (assoc lot :current-price (->> (:commodity lot)
;                                  prices/most-recent
;                                  :price)))
;
; (defn- transform-lot-transactions
;   [trans]
;   (map #(merge % (select-keys trans [:id :transaction-date]))
;        (:lot-items trans)))
;
; (defn- append-lot-transactions
;   [lot]
;   (assoc lot
;          :transactions
;          (mapcat transform-lot-transactions
;                  (transactions/search
;                   {[:lot-transaction :lot-id] (:id lot)
;                    :transaction-date [:>= (:purchase-date lot)]}
;                   {:include-lot-items? true}))))
;
; (defn- append-lot-calculated-values
;   [lot]
;   (let [cost (* (:shares-owned lot) (:purchase-price lot))
;         value (* (:shares-owned lot) (:current-price lot))
;         gain (- value cost)]
;     (assoc lot
;            :cost cost
;            :value value
;            :gain gain)))
;
; (defn lot-report
;   ([account-id]
;    (lot-report account-id nil))
;   ([account-id commodity-id]
;    (->> (lots/search (cond-> {:account-id account-id}
;                        commodity-id
;                        (assoc :commodity-id commodity-id)))
;         (map #(-> %
;                   append-commodity
;                   append-current-price
;                   (dissoc :commodity)
;                   append-lot-transactions
;                   append-lot-calculated-values))
;         (sort-by :caption)
;         (map #(dissoc % :id :shares-purchased :updated-at :created-at :account-id)))))
;
; (defn- append-portfolio-accounts
;   [{:keys [lots] :as ctx}]
;   (assoc ctx :accounts (if (seq lots)
;                          (->> (accounts/search {:id (set (map :account-id lots))})
;                               (map (juxt :id identity))
;                               (into {}))
;                          {})))
;
; (defn- append-commodities
;   [{:keys [lots] :as ctx}]
;   (assoc ctx :commodities (if (seq lots)
;                             (->> (commodities/search
;                                   {:id (->> lots
;                                             (map :commodity-id)
;                                             set)})
;                                  (map (juxt :id identity))
;                                  (into {}))
;                             {})))
;
; (defn- calc-gains
;   [{:keys [current-value current-shares purchase-price] :as lot}]
;   (let [cost-basis (* current-shares purchase-price)
;         gain-loss (- current-value cost-basis)]
;     (assoc lot
;            :cost-basis cost-basis
;            :gain-loss gain-loss
;            :gain-loss-percent (when-not (zero? cost-basis)
;                                 (with-precision 2
;                                   (/ gain-loss cost-basis))))))
;
; (defn- dispatch-portfolio-fn
;   [ctx]
;   {:pre [(:aggregate ctx)]}
;   (:aggregate ctx))
;
; (defmulti calc-portfolio-values dispatch-portfolio-fn)
;
; (defmethod calc-portfolio-values :by-account
;   [{:keys [lots] :as ctx}]
;   (assoc ctx
;          :report
;          (->> lots
;               (map calc-gains)
;               (group-by :account-id)
;               (map #(update-in % [1] (fn [lots] (group-by :commodity-id lots)))))))
;
; (defmethod calc-portfolio-values :by-commodity
;   [{:keys [lots] :as ctx}]
;   (assoc ctx
;          :report
;          (->> lots
;               (map calc-gains)
;               (group-by :commodity-id))))
;
; (defn- sum-fields
;   [target fields coll]
;   (reduce #(assoc %1 %2 (sum %2 coll))
;           target
;           fields))
;
; (defn- calc-gain-loss-percent
;   [{:keys [cost-basis gain-loss] :as target}]
;   (assoc target :gain-loss-percent (when-not (zero? cost-basis)
;                                      (with-precision 2
;                                        (/ gain-loss cost-basis)))))
;
; (defn- summarize-gains
;   ([target coll]
;    (summarize-gains target
;                     [:cost-basis
;                      :current-value
;                      :gain-loss]
;                     coll))
;   ([target fields coll]
;    (-> target
;        (sum-fields fields coll)
;        calc-gain-loss-percent)))
;
; (defn- flatten-and-summarize-commodity
;   [[commodity-id lots] {:keys [commodities]}]
;   (->> lots
;        (map #(assoc %
;                     :parents #{commodity-id}
;                     :style :data
;                     :caption (dates/format-local-date (:purchase-date %))))
;        (sort-by :purchase-date t/after?)
;        (cons (summarize-gains {:caption (get-in commodities [commodity-id :name])
;                                :id commodity-id
;                                :style :subheader}
;                               [:current-shares
;                                :cost-basis
;                                :current-value
;                                :gain-loss]
;                               lots))))
;
; (defn- flatten-and-summarize-account
;   [[account-id groups] {:keys [accounts balances] :as ctx}]
;   (let [cash-value (get-in balances [account-id :balance] 0M)
;         children (cons
;                   {:caption "Cash"
;                    :style :subheader
;                    :current-value cash-value
;                    :cost-basis cash-value
;                    :gain-loss 0M}
;                   (mapcat #(flatten-and-summarize-commodity % ctx)
;                           groups))]
;     (->> children
;          (map #(update-in % [:parents] (fnil conj #{}) account-id))
;          (cons (summarize-gains {:caption (get-in accounts [account-id :name])
;                                  :style :header
;                                  :id account-id}
;                                 (filter #(= :subheader (:style %)) children))))))
;
; (defn- append-portfolio-summary
;   [records]
;   (concat records
;           [(summarize-gains
;             {:caption "Total"
;              :style :summary
;              :id :summary}
;             (remove :parents records))]))
;
; (defmulti flatten-and-summarize-portfolio dispatch-portfolio-fn)
;
; (defmethod flatten-and-summarize-portfolio :by-commodity
;   [{:keys [commodities balances] :as ctx}]
;   (let [cash-value (->> (vals balances)
;                         (map :balance)
;                         (reduce + 0M))]
;     (update-in ctx
;                [:report]
;                (fn [report]
;                  (->> report
;                       (sort-by #(get-in commodities [(first %) :name]))
;                       (mapcat #(flatten-and-summarize-commodity % ctx))
;                       (cons {:caption "Cash"
;                              :style :subheader
;                              :current-shares cash-value
;                              :cost-basis cash-value
;                              :current-value cash-value
;                              :gain-loss 0M
;                              :gain-loss-percent 0M})
;                       (remove #(zero? (:current-value %)))
;                       append-portfolio-summary
;                       (map #(rename-keys % {:current-shares :shares-owned}))
;                       (map #(select-keys % [:caption
;                                             :style
;                                             :shares-purchased
;                                             :shares-owned
;                                             :cost-basis
;                                             :current-value
;                                             :gain-loss
;                                             :gain-loss-percent
;                                             :id
;                                             :parents])))))))
;
; (defmethod flatten-and-summarize-portfolio :by-account
;   [{:keys [accounts] :as ctx}]
;   (update-in ctx
;              [:report]
;              (fn [report]
;                (->> report
;                     (sort-by #(get-in accounts [(first %) :name]))
;                     (mapcat #(flatten-and-summarize-account % ctx))
;                     (remove #(zero? (:current-value %)))
;                     append-portfolio-summary
;                     (map #(rename-keys % {:current-shares :shares-owned}))
;                     (map #(select-keys % [:caption
;                                           :style
;                                           :shares-purchased
;                                           :shares-owned
;                                           :cost-basis
;                                           :current-value
;                                           :gain-loss
;                                           :gain-loss-percent
;                                           :id
;                                           :parents]))))))
;
; (defn portfolio
;   "Returns a portfolio report based on the options:
;     entity-id - Identifies the entity for which a report is to be generated (required)
;     as-of     - The date for which a report is to be generated. Defaults to today
;     aggregate - The method, :by-commodity or :by-account, defaults to :by-commodity"
;   [options]
;   {:pre [(:entity options)]}
;
;   (-> (merge {:as-of (t/local-date)
;               :aggregate :by-commodity}
;              options)
;       append-lots
;       update-lots
;       append-portfolio-accounts
;       append-balances
;       append-commodities
;       calc-portfolio-values
;       flatten-and-summarize-portfolio
;       :report))
