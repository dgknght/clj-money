(ns clj-money.reports
  (:require [clojure.set :refer [rename-keys
                                 intersection]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.inflection :refer [humanize]]
            [clj-money.find-in-chunks :as ch]
            [clj-money.models :as models]
            [clj-money.util :as util :refer [earliest
                                             model=]]
            [clj-money.dates :as dates]
            [clj-money.accounts :as accounts :refer [nest
                                                     unnest
                                                     left-side?
                                                     system-tagged?
                                                     valuate]]
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

(defn- asset?
  [{:report/keys [type]}]
  (= :asset type))

(defn- liability?
  [{:report/keys [type]}]
  (= :liability type))

(def ^:private balance-sheet?
  (some-fn asset? liability? equity?))

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

(defn- fetch-trx-items
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

(defn- balance-data
  [{:keys [accounts since as-of entity]}]
  (let [earliest-date (get-in entity [:entity/settings :settings/earliest-transaction-date])
        account-ids (->> accounts
                         (map :id)
                         set)
        start-balances (when since
                         (fetch-trx-items
                           account-ids
                           since
                           :find-one-fn (partial last-item :before since)
                           :earliest-date earliest-date))
        end-balances (fetch-trx-items
                       account-ids
                       as-of
                       :earliest-date earliest-date)]
    (->> account-ids
         (map (fn [id]
                [id (- (get-in end-balances [id :transaction-item/balance] 0M)
                       (get-in start-balances [id :transaction-item/balance] 0M))]))
         (into {}))))

(defn- valuation-data
  [{:keys [entity accounts as-of] :as options}]
  (let [balances (balance-data options)
        trading-account-ids (->> accounts
                                 (filter (system-tagged? :trading))
                                 (map :id))
        lots (when (seq trading-account-ids)
               (group-by (juxt (comp :id :lot/account)
                               (comp :id :lot/commodity))
                         (models/select {:lot/account [:in trading-account-ids]
                                         :lot/purchase-date [:<= as-of]})))
        lot-items (when (seq lots)
                    (group-by (comp :id :lot-item/lot)
                              (models/select {:lot-item/lot [:in (->> (vals lots)
                                                                      (mapcat identity)
                                                                      (mapv :id))]
                                              :lot-item/transaction-date [:<= as-of]})))
        prices (atom {})]
    (reify accounts/ValuationData
      (fetch-entity [& _] entity)
      (fetch-balance [_ {:keys [id]}] (balances id))
      (fetch-lots [_ {:account/keys [parent commodity]}]
        (lots (mapv :id [parent commodity])))
      (fetch-lot-items [_ {:keys [id]}]
        (lot-items id))
      (fetch-price [_ {:keys [id] :as commodity}]
        (if-let [cached (get-in @prices [id])]
          cached
          (let [price (:price/price (prices/most-recent commodity as-of))]
            (when-not price
              (throw (ex-info "No price found for commodity" {:commodity commodity})))
            (swap! prices assoc id price)
            price))))))

(defn- valuate-accounts
  "Given a sequence of accounts, assess their value based on the given date or
  range of dates.

  For accounts tracking the default commodity for the entity, the value comes
  from the balance of the transaction items that bound the date range.

  For accounts that track other commodities, the value comes from the quantity
  of that commodity held in the account and the most recent price based on the
  specified date."
  [opts accounts]
  (let [data (valuation-data (assoc opts :accounts accounts))]
    (valuate data accounts)))

(defn- apply-account-valuations
  [ctx]
  (update-in ctx [:accounts] (partial valuate-accounts ctx)))

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

(defn apply-account-summarization
  [{:keys [accounts] :as ctx}]
  (assoc ctx :records (mapcat summarize-account-type accounts)))

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

(defn- default-income-statement-range []
  (let [start (dates/first-day-of-the-month (t/local-date))]
    [start (t/plus start (t/months 1))]))

(defn income-statement
  "Returns the data used to populate an income statement report"
  ([entity]
   (let [[since as-of] (default-income-statement-range)]
     (income-statement entity since as-of)))
  ([entity since as-of]
   (->> (models/select #:account{:entity entity
                                 :type [:in #{:income :expense}]})
        (valuate-accounts {:entity entity
                           :since since
                           :as-of as-of})
        (nest {:types [:income :expense]})
        (mapcat summarize-account-type)
        summarize-income-statement)))

(defn- update-equity-total
  [records delta]
  (mapv (fn [r]
          (if (equity-header? r)
            (update-in r [:report/value] + delta)
            r))
        records))

(defn- append-equity-record
  [records value caption]
  (-> records
      vec
      (conj {:report/caption caption
             :report/style :data
             :report/type :equity
             :report/depth 0
             :report/value value})
      (update-equity-total value)))

(defn- calc-retained-earnings
  [records]
  (let [{:keys [income expense]} (map-record-headers records)]
    (- income expense)))

(defn- append-retained-earnings
  [records]
  (let [earnings (calc-retained-earnings records)]
    (if (zero? earnings)
      records
      (append-equity-record
        records
        earnings
        "Retained Earnings"))))

(defn- calc-unrealized-gains
  [{:keys [accounts]}]
  (->> accounts
       (map :account/gains)
       (filter identity)
       (reduce + 0M)))

(defn- append-unrealized-gains
  [records gains]
  (append-equity-record
    records
    gains
    "Unrealized Gains"))

(defn- apply-unrealized-gains
  [ctx]
  (let [gains (calc-unrealized-gains ctx)]
    (if (zero? gains)
      ctx
      (update-in ctx [:records] append-unrealized-gains gains))))

(defn- calc-liabilities-plus-equity
  [records]
  (let [{:keys [liability equity]} (map-record-headers records)]
    (+ liability equity )))

(defn- append-balance-sheet-summary
  [records]
  (conj records
        {:report/caption "Liabilities + Equity"
         :report/type :equity
         :report/style :summary
         :report/value (calc-liabilities-plus-equity records)}))

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
       apply-account-valuations
       (update-in [:accounts] nest)
       apply-account-summarization
       (update-in [:records] (comp (partial filterv balance-sheet?)
                                   append-retained-earnings))
       (update-in [:accounts] unnest)
       apply-unrealized-gains
       (update-in [:records] append-balance-sheet-summary)
       :records
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
       (valuate-accounts {:entity entity
                          :since (:budget/start-date budget)
                          :as-of as-of})
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
  [accounts entity since as-of]
  {:pre [since as-of (t/before? since as-of)]}
  (->> accounts
       (valuate-accounts {:since since
                          :as-of as-of
                          :entity entity})
       (map :account/value)
       (reduce + 0M)))

(defn- monitor-from-item
  [{:keys [account as-of budget children] {:keys [periods]} :item}]
  (let [period (budgets/period-containing budget as-of)
        period-budget (nth periods (:index period) 0M)
        total-budget (reduce + 0M periods)
        percent-of-period (budgets/percent-of-period budget
                                                     as-of)
        period-actual (aggregate-account-actuals (conj children account)
                                                 (:budget/entity budget)
                                                 (:start period)
                                                 as-of)
        total-actual (aggregate-account-actuals (conj children account)
                                                (:budget/entity budget)
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
                           :budget (update-in budget
                                              [:budget/entity]
                                              (models/find :entity))})
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

(defn- valuate-lot
  [current-price]
  (fn [{:as lot :lot/keys [shares-owned purchase-price]}]
    (let [cost-basis (* purchase-price shares-owned)
          current-value (* current-price shares-owned)]
      (assoc lot
             :lot/cost-basis cost-basis
             :lot/current-value current-value
             :lot/gain (- current-value cost-basis)))))

(defn- valuate-lots
  [commodity lots]
  (let [current-price (:price/price (prices/most-recent commodity))
        adj-lots (map (valuate-lot current-price) lots)
        shares-owned (sum :lot/shares-owned adj-lots)
        current-value (* shares-owned current-price)
        cost-basis (sum :lot/cost-basis adj-lots)]
    (assoc commodity
           :commodity/current-price current-price
           :commodity/current-value current-value
           :commodity/shares-owned shares-owned
           :commodity/cost-basis cost-basis
           :commodity/gain (- current-value cost-basis)
           :commodity/lots adj-lots)))

(defn lot-report
  ([account]
   (lot-report account nil))
  ([account commodity]
   (->> (models/select (cond-> {:lot/account account
                                :lot/shares-owned [:> 0]}
                         commodity
                         (assoc :lot/commodity commodity))
                       {:sort [[:lot/purchase-date :asc]]})
        (map #(assoc % :lot/items (models/select {:lot-item/lot %}
                                                 {:sort [[:lot-item/transaction-date :asc]]})))
        (group-by (comp :id :lot/commodity))
        (map (comp #(apply valuate-lots %)
                   #(update-in % [0] (fn [id] (models/find id :commodity)))))
        (sort-by :commodity/name))))

(declare aggregate-portfolio-account)

(defmulti ^:private aggregate-portfolio-children
  (fn [m _]
    (cond
      (:account/children m) :account
      (:account/lots m)     :lot
      (:lot/items m)        :lot-item)))

(defmethod aggregate-portfolio-children :account
  [{:account/keys [children]} depth]
  (mapcat #(aggregate-portfolio-account % :depth (inc depth))
          children))

(defmethod aggregate-portfolio-children :lot
  [{:account/keys [lots]} depth]
  (map (fn [{:lot/keys [purchase-date shares-owned value cost-basis gain]}]
         {:report/caption (dates/format-local-date purchase-date)
          :report/depth depth
          :report/style :data
          :report/shares-owned shares-owned
          :report/current-value value
          :report/cost-basis cost-basis
          :report/gain-loss gain
          :report/gain-loss-percent (with-precision 3
                                      (/ gain cost-basis))})
       lots))

(defn- format-commodity
  [{:commodity/keys [name symbol]}]
  (format "%s (%s)" name symbol))

(defn- aggregate-portfolio-account
  [{:account/keys [name
                   shares-owned
                   total-value
                   cost-basis
                   value
                   commodity
                   gain]
    :as account}
   & {:keys [depth] :or {depth 0}}]
  {:pre [(:account/total-value account)
         (:account/value account)
         (:account/cost-basis account)]}
  (let [shared {:report/caption name
                :report/depth depth
                :report/cost-basis cost-basis
                :report/gain-loss gain
                :report/gain-loss-percent (with-precision 3
                                            (/ gain cost-basis))
                :report/shares-owned shares-owned
                :report/current-value total-value}]
    (if (system-tagged? account :trading)
      (cons (merge shared
                   {:report/style :header
                    :report/caption name})
            (cons {:report/caption "Cash"
                   :report/style :subheader
                   :report/current-value value
                   :report/cost-basis value
                   :report/gain-loss 0M}
                  (aggregate-portfolio-children account depth)))
      (cons (merge shared
                   {:report/style :subheader
                    :report/caption (format-commodity commodity)})
            (aggregate-portfolio-children account depth)))))

(defn- aggregate-portfolio-by-account
  [accounts]
  (->> accounts
       nest
       (mapcat :accounts)
       (mapcat aggregate-portfolio-account)))

(defn- aggregate-portfolio-commodity
  [[_commodity-id accounts]]
  (let [commodity (:account/commodity (first accounts))
        current-value (sum :account/value accounts)]
    (if (:commodity/default? commodity)
      (cons {:report/caption (format-commodity commodity)
             :report/style :header
             :report/current-value current-value
             :report/cost-basis current-value
             :report/gain-loss 0M
             :report/gain-loss-percent 0.0M}
            (map (fn [account]
                   {:report/caption (:account/name account)
                    :report/style :data
                    :report/current-value (:account/value account)})
                 accounts))
      (let [cost-basis (sum :account/cost-basis accounts)
            gain (- current-value cost-basis)]
        (cons {:report/caption (format-commodity commodity)
               :report/style :header
               :report/shares-owned (sum :account/shares-owned accounts)
               :report/current-value current-value
               :report/cost-basis cost-basis
               :report/gain-loss gain
               :report/gain-loss-percent (with-precision 3
                                           (/ gain cost-basis))}
              (->> accounts
                   (mapcat :account/lots)
                   (sort-by :lot/purchase-date t/after?)
                   (map (fn [{:lot/keys [shares-owned cost-basis gain purchase-date value]}]
                          {:report/caption (dates/format-local-date purchase-date)
                           :report/style :data
                           :report/shares-owned shares-owned
                           :report/cost-basis cost-basis
                           :report/current-value value
                           :report/gain-loss gain
                           :report/gain-loss-percent (with-precision 3
                                                       (/ gain cost-basis))}))))))))

(defn- commodity->sortable
  [{:commodity/keys [name]}]
  ({"Cash" "*"} name name))

(defn- aggregate-portfolio-by-commodity
  [accounts]
  (->> accounts
       (group-by :account/commodity)
       (sort-by (comp commodity->sortable first))
       (mapcat aggregate-portfolio-commodity)))

(defn- add-seqs
  [s1 s2]
  (->> s1
       (interleave s2)
       (partition 2)
       (map (partial apply +))))

(defn- append-portfolio-total
  [records]
  (let [[cost-basis current-value] (->> records
                                        (filter #(= :header (:report/style %)))
                                        (map (juxt :report/cost-basis
                                                   :report/current-value))
                                        (reduce add-seqs [0M 0M]))
        gain-loss (- current-value cost-basis)]
    (conj (vec records)
          {:report/style :summary
           :report/caption "Total"
           :report/current-value current-value
           :report/cost-basis cost-basis
           :report/gain-loss gain-loss
           :report/gain-loss-percent (with-precision 3
                                       (/ gain-loss cost-basis))})))

(defn portfolio
  "Returns a portfolio report based on the options:
    entity    - The entity for which a report is to be generated (required)
    as-of     - The date for which a report is to be generated. Defaults to today
    aggregate - The method, :by-commodity or :by-account, defaults to :by-commodity"
  [{:keys [entity] :as options}]
  {:pre [(:entity options)]}
  (let [accounts (models/select {:account/entity entity
                                 :account/type :asset
                                 #_:account/system-tags #_[:&& #{:trading :tradable}]})
        commodities (->> (models/select {:commodity/entity entity})
                         (map (fn [c]
                                (if (model= c
                                            (get-in entity [:entity/settings
                                                            :settings/default-commodity]))
                                  (assoc c
                                         :commodity/name "Cash"
                                         :commodity/default? true)
                                  c)))
                         (index-by :id))
        aggregate (case (:aggregate options)
                    :by-account aggregate-portfolio-by-account
                    :by-commodity aggregate-portfolio-by-commodity
                    (throw (IllegalArgumentException. (str "Unknown aggregate type: " (:aggregate options)))))]
    (->> accounts
         (filter (some-fn (system-tagged? :tradable)
                          (system-tagged? :trading))) ; TODO: Make the system tag query above work
         (map #(update-in % [:account/commodity] (comp commodities :id)))
         (valuate-accounts options)
         (aggregate)
         (append-portfolio-total))))
