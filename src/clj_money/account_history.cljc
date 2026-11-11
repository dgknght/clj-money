(ns clj-money.account-history
  "Shapes summaries of account activity into charts of account history"
  (:require [clojure.string :as string]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.dates :as dates]))

(def period-types
  [:year :quarter :month :week])

(def measures
  "What is charted for each period: the activity in the account during the
  period, or the balance of the account at the end of the period"
  [:activity :balance])

(def units
  "How each measure is expressed: as the value in the entity's default
  commodity, or as the quantity of the commodity the account tracks (shares,
  for a stock or fund account). The two are the same for an account that
  tracks the default commodity."
  [:value :quantity])

(def ^:private month-abbrs
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defn- month-number
  [date]
  #?(:clj (.getValue (t/month date))
     :cljs (t/month date)))

(defn- quarter
  [date]
  (inc (quot (dec (month-number date)) 3)))

(defn- year
  [date]
  #?(:clj (.getValue (t/year date))
     :cljs (t/year date)))

(defn- start-of-period
  [date period-type]
  (case period-type
    :year (dates/first-day-of-the-year date)
    :quarter (dates/first-day-of-the-month (year date)
                                           (inc (* 3 (dec (quarter date)))))
    :month (dates/first-day-of-the-month date)
    :week (dates/first-day-of-the-week date)))

(defn criteria
  "Returns the criteria for summarizing the activity in the account by
  period, with the start date moved back to the start of its period so that
  the summarized periods line up with calendar periods"
  [account {:keys [start-date end-date period-type]}]
  {:transaction/transaction-date [(start-of-period start-date period-type)
                                  (t/plus end-date (t/days 1))] ; the end of the range is exclusive
   :transaction-item/account account
   :period [1 period-type]
   :include-balance true
   :include-value true})

(defn- period-label
  [date period-type]
  (case period-type
    :year (str (year date))
    :quarter (str "Q" (quarter date) " " (year date))
    :month (str (get month-abbrs (dec (month-number date))) " " (year date))
    :week (dates/format-local-date date)))

(defn- ->number
  "Converts the decimal to a number for the chart, leaving nil (a period that
  can't be valued, which the chart leaves blank) alone"
  [quantity]
  (when quantity
    #?(:clj (double quantity)
       :cljs (.toNumber quantity))))

(def ^:private summary-keys
  {[:activity :quantity] :quantity
   [:activity :value]    :value
   [:balance :quantity]  :balance
   [:balance :value]     :balance-value})

(defn- dataset
  [measure unit]
  (let [k (summary-keys [(or measure :activity)
                         (or unit :value)])]
    (fn [{:keys [account summary]}]
      {:label (string/join "/" (:account/path account))
       :data (mapv (comp ->number k) summary)})))

(defn chart-config
  "Returns the configuration for a chart of the given account summaries,
  each of which is a map containing the :account and its :summary, as
  returned by the summarize endpoint for the criteria above.

  When :stacked? is true and the style is :bar, the accounts' bars for each
  period are stacked on top of one another instead of drawn side by side."
  [summaries {:keys [period-type measure unit style show-values? stacked?]}]
  (let [stacked? (and stacked? (= :bar style))
        ; a label above a stacked segment would cover the segment above it
        label-position (if stacked? "center" "end")]
    (cond-> {:type style
             :data {:labels (->> (first summaries) ; every summary covers the same periods
                                 :summary
                                 (mapv #(period-label (:start-date %) period-type)))
                    :datasets (mapv (dataset measure unit) summaries)}
             :options {:plugins {:datalabels {:display show-values?
                                              :anchor label-position
                                              :align label-position}}}}
      stacked? (assoc-in [:options :scales] {:x {:stacked true}
                                             :y {:stacked true}}))))
