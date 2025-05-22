(ns clj-money.budgets
  (:require [clojure.string :as string]
            [clojure.set :refer [intersection]]
            [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            #?(:clj [clojure.math :as math])
            [dgknght.app-lib.inflection :refer [title-case]]
            [clj-money.decimal :as d]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.accounts :as acts]
            [clj-money.transactions :as txns]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            #?(:cljs [cljs-time.format :as tf])
            #?(:cljs [cljs-time.coerce :as tc])))

(def periods #{:week :month :quarter})

(s/def ::total d/decimal?)
(s/def ::average d/decimal?)
(s/def ::amount d/decimal?)
(s/def ::week-count integer?)
(s/def ::start-date dates/local-date?)
(s/def ::round-to integer?)
(s/def ::item-spec (s/or :per-total (s/keys :req-un [::total])
                         :per-average (s/keys :req-un [::average])
                         :per-week (s/keys :req-un [::start-date
                                                    ::amount
                                                    ::week-count])
                         :historical (s/keys :req-un [::start-date
                                                      ::round-to])))

(defmulti period-description
  (fn [_ budget]
    (get-in budget [:budget/period 1])))

(defmethod period-description :month
  [index {:budget/keys [start-date]}]
  #?(:clj (t/format (t/formatter "MMM YYYY") (t/plus start-date (t/months index)))
     :cljs (tf/unparse (tf/formatter "MMM YYYY")
              (tc/to-date-time (t/plus start-date (t/months index))))))

(defmethod period-description :quarter
  [index {:budget/keys [start-date]}]
  (str "Q" (inc index) " " (t/year start-date)))

(defn- sum
  [coll]
  #?(:clj (reduce + coll)
     :cljs (reduce d/+ coll)))

(defn- render-section
  [items caption filter-fn]
  (let [sections (->> items
                      (filter filter-fn)
                      (sort-by (comp :account/path :budget-item/account))
                      (map (fn [{:as item :budget-item/keys [account periods]}]
                             #:budget-section{:item item
                                              :caption (string/join "/" (:account/path account))
                                              :total (sum periods)})))]
    #:budget-section{:caption caption
                     :items sections
                     :total (->> sections
                                 (map :budget-section/total)
                                 sum)
                     :periods (->> sections
                                   (map (comp :budget-item/periods :budget-section/item))
                                   (apply interleave)
                                   (partition (count sections))
                                   (map #(sum %)))}))

(defn- income?
  [{:budget-item/keys [account]}]
  (= :income (:account/type account)))

; For the purposes of a budget, anything that is not income is outflow
(def expense? (complement income?))

(defn- render-income
  [items]
  (render-section items "Income" income?))

(defn- render-expense
  [items]
  (render-section items "Expenses" expense?))

(defn- item-tagged?
  [tag]
  (fn [{:budget-item/keys [account]}]
    (acts/user-tagged? account tag)))

(defn- item-not-tagged?
  [tags]
  (fn [{:budget-item/keys [account]}]
    (empty? (intersection tags (:account/user-tags account)))))

(defn- subtract-periods
  [& sections]
  (->> sections
       (map :budget-section/periods)
       (apply interleave)
       (partition 2)
       (map #(apply - %))))

(defn- append
  [item coll]
  (concat coll
          [item]))

(defn- process-tagged-items
  [items]
  (fn [acc {:budget-section/keys [caption pred summary-caption]}]
    (let [section (render-section items caption pred)
          net-periods (subtract-periods (last acc) section)]
      (if (seq (:budget-section/items section))
        (conj acc
              section
              #:budget-section{:caption summary-caption
                               :periods net-periods
                               :total (sum net-periods)})
        acc))))

(defn- render-tagged
  [items tags]
  (->> tags
       (map (fn [tag]
              #:budget-section{:caption (str (title-case tag) " Expenses")
                               :pred (every-pred expense?
                                                 (item-tagged? tag))
                               :summary-caption (str "Available After " (title-case tag))}))
       (append #:budget-section{:caption "Uncategorized"
                                :pred (every-pred expense?
                                                  (item-not-tagged? (set tags)))
                                :summary-caption "Net"})
       (reduce (process-tagged-items items)
               [(render-income items)])))

(defn- render-untagged
  [items]
  (let [income (render-income items)
        expense (render-expense items)
        net-periods (subtract-periods income expense)
        net #:budget-section{:caption "Net"
                             :periods net-periods
                             :total (sum net-periods)}]
    [income
     expense
     net]))

(defn render
  "Given a budget and a fn to look up accounts by id,
  returns a structure with summarized totals by inflow and outflow"
  [{:budget/keys [items]} {:keys [find-account tags] :as options}]
  {:pre [(:find-account options)]}

  (let [items (map #(update-in % [:budget-item/account] (comp find-account :id))
                   items)]
    (if (seq tags)
      (render-tagged items tags)
      (render-untagged items))))

(defn- trx-items->budget-item
  [{[_ period-type] :budget/period}
   since
   as-of]
  (fn [[account trx-items]]
    #:budget-item{:account account
                  :periods (->> trx-items
                                (txns/summarize-items {:period [1 period-type]
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
  [{:as budget
    :budget/keys [start-date]
    [period-count period-type] :budget/period}]
  {:pre [(:budget/start-date budget)
         (:budget/period budget)]}

  (when budget
    (->> (dates/periodic-seq start-date
                             (get-in period-map
                                     [period-type]
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

(defn- round
  [number places]
  #?(:clj (let [factor (math/pow 10.0 places)]
            (/ (math/round (* factor number))
               factor))
     :cljs (let [factor (Math/pow 10 places)]
             (/ (Math/round (* factor number))
                factor))))

(defn percent-of-period
  [budget as-of]
  (let [period (period-containing budget as-of)
        days-in-period (inc (dates/days-between (:start period) (:end period)))
        days (inc (dates/days-between (:start period)
                                      as-of))]
    #?(:clj (with-precision 5 (/ days days-in-period))
       :cljs (round (/ days days-in-period)
                    3))))

(defn find-item-by-account
  "Finds the item in the specified budget associated with the specified account"
  [{:budget/keys [items]} {:keys [id]}]
  (->> items
       (filter #(= id
                   (get-in % [:budget-item/account :id])))
       first))

(defn- to-chan
  [x]
  (let [ch (a/promise-chan)]
    (a/go (a/>! ch x))
    ch))

(defmulti calc-periods
  (fn [{:budget-item/keys [spec]} & _]
    (let [conformed (s/conform ::item-spec spec)]
      (if (= ::s/invalid conformed)
        (throw (ex-info "Unrecognized budget item spec." {:spec spec}))
        (first conformed)))))

(defmethod calc-periods :per-average
  [{{:keys [average]} :budget-item/spec} {[period-count] :budget/period} & _]
  (to-chan (repeat period-count average)))

(defmethod calc-periods :per-total
  [{{:keys [total]} :budget-item/spec} {[period-count] :budget/period}]
  (let [amount (d// total (d/d period-count))]
    (to-chan (repeat period-count amount))))

(defmethod calc-periods :per-week
  [{{:keys [start-date week-count amount]} :budget-item/spec}
   {:budget/keys [end-date]}
   & _]
  (->> (dates/periodic-seq start-date
                           (t/weeks week-count))
       (take-while #(t/before? % end-date))
       (group-by t/month)
       (map #(update-in %
                        [1]
                        (comp (partial d/* amount)
                              d/d
                              count)))
       (sort-by first)
       (map second)
       to-chan))

(defmethod calc-periods :historical
  [{:budget-item/keys [account]
    {:keys [start-date round-to]} :budget-item/spec}
   {:as budget [_ period-type] :budget/period}
   & {:keys [fetch-item-summaries]}]
  (let [end-date (t/plus (end-date (assoc budget
                                          :budget/start-date
                                          start-date))
                         (t/days 1))
        out-chan (a/chan)]
    (a/go
      (a/pipeline 1
                  out-chan
                  (map (fn [s] (map (comp #(d/round % round-to)
                                          :quantity)
                                    s)))
                  (fetch-item-summaries
                    {:transaction-item/transaction-date [start-date end-date]
                     :transaction-item/account account
                     :period [1 period-type]})))
    out-chan))
