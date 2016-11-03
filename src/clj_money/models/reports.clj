(ns clj-money.models.reports
  (:require [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-money.inflection :refer [humanize]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(declare set-balance-deltas)
(defn- set-balance-delta
  [storage-spec account start end]
  (let [children (set-balance-deltas storage-spec start end (:children account))]
    (assoc account :children children
           :children-balance (reduce #(+ %1
                                         (:balance %2)
                                         (:children-balance %2))
                                     0
                                     children)
           :balance (transactions/balance-delta storage-spec
                                                (:id account)
                                                start
                                                end))))
(defn- set-balance-deltas
  [storage-spec start end nested-accounts]
  (map #(set-balance-delta storage-spec % start end)
       nested-accounts))

(defn- transform-account
  [account depth]
  (concat [{:caption (:name account)
            :value (+ (:balance account) (:children-balance account))
            :style :data
            :depth 0}]
          (map #(transform-account % (+ 1 depth))
               (:children account))))

(defn- transform-account-group
  [{:keys [type accounts]}]
  (concat [{:caption (humanize type)
            :value (reduce #(+ %1 (:balance %2) (:children-balance %2)) 0 accounts)
            :style :header}]
          (mapcat #(transform-account % 0) accounts)))

(defn- transform
  "Accepts a nested account structure and returns a report structure"
  [nested-accounts]
  (concat (mapcat transform-account-group
                  nested-accounts)
          [{:caption "Net"
            :value (bigdec 0)
            :style :summary}]))

(defn income-statement
  "Returns the data used to populate an income statement report"
  ([storage-spec entity-id]
   (let [base (t/today)
         start (t/local-date (t/year base) 1 1)
         end (t/local-date (t/year base) (t/month base) (t/number-of-days-in-the-month base))]
     (income-statement storage-spec entity-id start end)))
  ([storage-spec entity-id start end]
   (->> (accounts/select-nested-by-entity-id
          storage-spec
          entity-id
          [:income :expense])
        (set-balance-deltas storage-spec start end)
        transform)))
