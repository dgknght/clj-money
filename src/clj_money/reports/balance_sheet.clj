(ns clj-money.reports.balance-sheet
  (:require [clojure.tools.logging :as log]
            [java-time.api :as t]
            [clj-money.entities :as entities]
            [clj-money.accounts :refer [nest
                                        unnest
                                        system-tagged?]]
            [clj-money.reports :as rpt]))

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
  (every-pred rpt/header? equity?))

(defn- apply-account-valuations
  [ctx]
  (update-in ctx [:accounts] (partial rpt/valuate-accounts ctx)))

(defn- apply-account-summarization
  [{:keys [accounts] :as ctx}]
  (assoc ctx :records (mapcat rpt/summarize-account-type accounts)))

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
  (let [{:keys [income expense]} (rpt/map-record-headers records)]
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
       (filter (system-tagged? :tradable))
       (map :account/gain)
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
  (let [{:keys [liability equity]} (rpt/map-record-headers records)]
    (+ liability equity )))

(defn- append-summary
  [records]
  (conj records
        {:report/caption "Liabilities + Equity"
         :report/type :equity
         :report/style :summary
         :report/value (calc-liabilities-plus-equity records)}))

(defn- check-balance
  [records]
  (let [{:keys [asset liability equity]} (rpt/map-record-headers records)
        l-and-e (+ liability equity)]
    (when (> (abs (- asset l-and-e))
             0.01M)
      (log/warnf "Balance sheet out of balance. Asset: %s, Liability + Equity %s, difference %s"
                 asset
                 l-and-e
                 (- asset l-and-e))))
  records)

(defn report
  "Returns the data used to populate a balance sheet report"
  ([entity]
   (report entity (t/local-date)))
  ([entity as-of]
   (-> {:entity entity
        :as-of as-of
        :accounts (entities/select {:account/entity entity})}
       apply-account-valuations
       (update-in [:accounts] nest)
       apply-account-summarization
       (update-in [:records] (comp (partial filterv balance-sheet?)
                                   append-retained-earnings))
       (update-in [:accounts] unnest)
       apply-unrealized-gains
       (update-in [:records] append-summary)
       :records
       check-balance)))
