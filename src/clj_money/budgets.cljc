(ns clj-money.budgets
  (:require [clojure.string :as string]
            [clojure.set :refer [intersection]]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [dgknght.app-lib.inflection :refer [title-case]]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.accounts :as acts]
            [clj-money.transactions :as txns]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            #?(:cljs [cljs-time.format :as tf])
            #?(:cljs [cljs-time.coerce :as tc])
            #?(:cljs [dgknght.app-lib.decimal :as decimal])))

(def periods #{:month :quarter :year})

(defmulti period-description
  (fn [_ budget]
    (:period budget)))

(defmethod period-description :month
  [index {:keys [start-date]}]
  #?(:clj (t/format (t/formatter "MMM YYYY") (t/plus start-date (t/months index)))
     :cljs (tf/unparse (tf/formatter "MMM YYYY")
              (tc/to-date-time (t/plus start-date (t/months index))))))

(defmethod period-description :quarter
  [index {:keys [start-date]}]
  (str "Q" (inc index) " " (t/year start-date)))

(defn- sum
  [coll]
  #?(:clj (reduce + coll)
     :cljs (decimal/sum coll)))

(defn- render-items
  [items caption filter-fn]
  (let [rendered-items (->> items
                            (filter filter-fn)
                            (sort-by (comp :path :account))
                            (map (fn [item]
                                   {:item (dissoc item :account) ; TODO: should I remove this here? It simplifies testing, but is it better?
                                    :caption (string/join "/" (-> item :account :path))
                                    :total (sum (:periods item))})))]
    {:caption caption
     :items rendered-items
     :total (->> rendered-items
                 (map :total)
                 sum)
     :periods (->> rendered-items
                   (map (comp :periods :item))
                   (apply interleave)
                   (partition (count rendered-items))
                   (map #(sum %)))}))

(defn- income?
  [{:keys [account]}]
  (= :income (:type account)))

; For the purposes of a budget, anything that is not income is outflow
(def expense? (complement income?))

(defn- render-income
  [items]
  (render-items items "Income" income?))

(defn- render-expense
  [items]
  (render-items items "Expenses" expense?))

(defn- item-tagged?
  [tag {:keys [account]}]
  (acts/user-tagged? account tag))

(defn- item-not-tagged?
  [tags {:keys [account]}]
  (empty? (intersection tags (:user-tags account))))

(defn- subtract-periods
  [& groups]
  (->> groups
       (map :periods)
       (apply interleave)
       (partition 2)
       (map #(apply - %))))

(defn- append
  [item coll]
  (concat coll
          [item]))

(defn- process-tagged-items
  [items result {:keys [caption pred summary-caption]}]
  (let [group (render-items items caption pred)
        net-periods (subtract-periods (last result) group)]
    (if (seq (:items group))
      (concat result
              [group
               {:caption summary-caption
                :periods net-periods
                :total (sum net-periods)}])
      result)))

(defn- render-tagged
  [items tags]
  (->> tags
       (map (fn [tag]
              {:caption (str (title-case tag) " Expenses")
               :pred (every-pred expense?
                                 (partial item-tagged? tag))
               :summary-caption (str "Available After " (title-case tag))}))
       (append {:caption "Uncategorized"
                :pred (every-pred expense?
                                  (partial item-not-tagged? (set tags)))
                :summary-caption "Net"})
       (reduce (partial process-tagged-items items)
               [(render-income items)])))

(defn- render-untagged
  [items]
  (let [income (render-income items)
        expense (render-expense items)
        net-periods (subtract-periods income expense)
        net {:caption "Net"
             :periods net-periods
             :total (sum net-periods)}]
    [income
     expense
     net]))

(defn render
  "Given a budget and a fn to look up accounts by id,
  returns a structure with summarized totals by inflow and outflow"
  [{:keys [items]} {:keys [find-account tags] :as options}]
  {:pre [(:find-account options)]}

  (let [items (map #(assoc % :account (find-account (:account-id %)))
                   items)]
    (if (seq tags)
      (render-tagged items tags)
      (render-untagged items))))

(defn- fetch-account
  [account-or-ref accounts]
  (if (util/model-ref? account-or-ref)
    (if-let [account (accounts (:id account-or-ref))]
      [account accounts]
      (let [account (models/find account-or-ref :account)]
        [account (assoc accounts (:id account) account)]))
    [account-or-ref accounts]))

(defn- realize-accounts
  [trx-items]
  (loop [input trx-items output [] accounts {}]
    (if-let [item (first input)]
      (let [[account accounts] (fetch-account (:transaction-item/account item)
                                              accounts)]
        (recur (rest input)
               (conj output
                     (assoc item
                            :transaction-item/account
                            account))
               accounts))
      output)))

(defn- trx-items->budget-item
  [{:budget/keys [period]}
   since
   as-of]
  (fn [[account trx-items]]
    #:budget-item{:account account
                  :periods (->> trx-items
                                realize-accounts
                                (txns/summarize-items {:interval-type period
                                                       :interval-count 1
                                                       :since since
                                                       :as-of as-of})
                                (mapv :quantity))}))

(defn create-items-from-history
  [budget since as-of trx-items]
  (->> trx-items
       (group-by (comp util/->model-ref
                       :transaction-item/account))
       (map (trx-items->budget-item budget
                                    since
                                    (t/minus as-of (t/days 1))))))

(def ^:private period-map
  {:month (t/months 1)
   :week (t/weeks 1)
   :quarter (t/months 3)})

(defn- period-seq
  "Returns a sequence of the java.time.Period instances in the budget based on
  :start-date, :period, :period-count"
  [{:as budget :budget/keys [start-date period-count period]}]
  {:pre [(:budget/start-date budget)
         (:budget/period-count budget)
         (:budget/period budget)]}

  (when budget
    (->> (dates/periodic-seq start-date
                             (get-in period-map
                                     [period]
                                     (t/months 1)))
         (partition 2 1)
         (map-indexed (fn [index [start next-start]]
                        {:start start
                         :end (t/minus next-start (t/days 1))
                         :index index
                         :interval (t/period start next-start)}))
         (take period-count))))

(defn end-date
  [budget]
  (-> budget
      period-seq
      last
      :end))

(defn- within-period?
  "Returns a boolean value indicating whether or not
  the specified date is in the specified period"
  [period date]
  (dates/within?
    date
    [(:start period)
     (:end period)]))

(defn period-containing
  "Returns the budget period containing the specified date

  This is a map containing :start-date, :end-date, :index, etc."
  [budget date]
  (->> (period-seq budget)
       (map-indexed #(assoc %2 :index %1))
       (filter #(within-period? % date))
       first))

(defn percent-of-period
  [budget as-of]
  (let [period (period-containing budget as-of)
        days-in-period (inc (dates/days-between (:start period) (:end period)))
        days (inc (dates/days-between (:start period)
                                      as-of))]
    #?(:clj (with-precision 5 (/ days days-in-period))
       :cljs (/ days days-in-period))))

(defn find-item-by-account
  "Finds the item in the specified budget associated with the specified account"
  [{:budget/keys [items]} {:keys [id]}]
  (->> items
       (filter #(= id
                   (get-in % [:budget-item/account :id])))
       first))
