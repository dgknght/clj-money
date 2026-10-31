(ns clj-money.account-history
  "Shapes summaries of account activity into charts of account history"
  (:require [clojure.string :as string]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.dates :as dates]))

(def period-types
  [:year :quarter :month :week])

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
   :period [1 period-type]})

(defn- period-label
  [date period-type]
  (case period-type
    :year (str (year date))
    :quarter (str "Q" (quarter date) " " (year date))
    :month (str (get month-abbrs (dec (month-number date))) " " (year date))
    :week (dates/format-local-date date)))

(defn- ->number
  [quantity]
  #?(:clj (double quantity)
     :cljs (.toNumber quantity)))

(defn- dataset
  [{:keys [account summary]}]
  {:label (string/join "/" (:account/path account))
   :data (mapv (comp ->number :quantity) summary)})

(defn chart-config
  "Returns the configuration for a chart of the given account summaries,
  each of which is a map containing the :account and its :summary, as
  returned by the summarize endpoint for the criteria above"
  [summaries {:keys [period-type style show-values?]}]
  {:type style
   :data {:labels (->> (first summaries) ; every summary covers the same periods
                       :summary
                       (mapv #(period-label (:start-date %) period-type)))
          :datasets (mapv dataset summaries)}
   :options {:plugins {:datalabels {:display show-values?
                                    :anchor "end"
                                    :align "end"}}}})
