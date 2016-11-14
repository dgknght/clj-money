(ns clj-money.models.reports
  (:require [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.inflection :refer [humanize]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]))

(declare set-balance-deltas)
(defn- set-balance-delta
  [storage-spec account start end]
  (let [children (set-balance-deltas storage-spec start end (:children account))
        new-children-balance (reduce #(+ %1
                                         (:balance %2)
                                         (:children-balance %2))
                                     0
                                     children)
        new-balance (transactions/balance-delta storage-spec
                                                (:id account)
                                                start
                                                end)]
    (assoc account :children children
                   :children-balance new-children-balance
                   :balance new-balance)))

; TODO consider removing this function
(defn- set-balance-deltas
  [storage-spec start end accounts]
  (map #(set-balance-delta storage-spec % start end) accounts))

; TODO combine this with set-balances-in-account-group
(defn- set-balance-deltas-in-account-group
  [storage-spec entry start end]
  (let [updated (update-in entry
                           [:accounts]
                           (partial set-balance-deltas storage-spec start end))]
    (assoc updated :value (reduce #(+ %1
                                      (:balance %2)
                                      (:children-balance %2))
                                  0
                                  (:accounts updated)))))

(defn- set-balance-deltas-in-account-groups
  [storage-spec start end groups]
  (map #(set-balance-deltas-in-account-group
          storage-spec
          %
          start
          end)
       groups))

(declare set-balances)
; TODO combine this with set-balance-delta
(defn- set-balance
  [storage-spec account as-of]
  (let [children (set-balances storage-spec as-of (:children account))
        new-children-balance (reduce #(+ %1
                                         (:balance %2)
                                         (:children-balance %2))
                                     0
                                     children)
        new-balance (transactions/balance-as-of storage-spec
                                                (:id account)
                                                as-of)]
    (assoc account :children children
                   :children-balance new-children-balance
                   :balance new-balance)))

; TODO consider removing this function
(defn- set-balances
  [storage-spec as-of accounts]
  (map #(set-balance storage-spec % as-of) accounts))

; TODO combine this with set-balance-deltas-in-account-group
(defn- set-balances-in-account-group
  [storage-spec entry as-of]
  (let [updated (update-in entry
                           [:accounts]
                           (partial set-balances storage-spec as-of))]
    (assoc updated :value (reduce #(+ %1
                                      (:balance %2)
                                      (:children-balance %2))
                                  (bigdec 0)
                                  (:accounts updated)))))

(defn- set-balances-in-account-groups
  [storage-spec as-of groups]
  (map #(set-balances-in-account-group storage-spec % as-of) groups))

(defn- transform-account
  [account depth]
  (concat [{:caption (:name account)
            :value (+ (:balance account) (:children-balance account))
            :style :data
            :depth depth}]
          (mapcat #(transform-account % (+ 1 depth))
               (:children account))))

(defn- transform-account-group
  [{:keys [type accounts value]}]
  (concat [{:caption (humanize type)
            :value value
            :style :header}]
          (mapcat #(transform-account % 0) accounts)))

(defn- transform-income-statement
  "Accepts grouped accounts structure and returns a report structure"
  [groups]
  (let [summary (->> groups
                     (map (juxt :type :value))
                     (into {}))]
    (concat (mapcat transform-account-group
                    groups)
            [{:caption "Net"
              :value (- (:income summary) (:expense summary))
              :style :summary}])))

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
        (into [])
        (set-balance-deltas-in-account-groups storage-spec start end)
        transform-income-statement)))

(defn- transform-balance-sheet
  "Accepts group accounts and returns a report structure"
  [groups]
  (let [summary (->> groups
                     (map (juxt :type :value))
                     (into {}))
        retained (- (:income summary) (:expense summary))]
    (->> groups
         (map (fn [entry]
                (if (= :equity (:type entry))
                  (-> entry 
                      (update-in [:accounts] #(conj % {:name "Retained Earnings"
                                                       :balance retained
                                                       :children-balance 0}))
                      (update-in [:value] #(+ %1 retained)))
                  entry)))
         (remove #(#{:income :expense} (:type %)))
         (mapcat transform-account-group))))

(defn balance-sheet
  "Returns the data used to populate a balance sheet report"
  ([storage-spec entity-id]
   (let [base (t/today)
         end (t/local-date (t/year base) (t/month base) (t/number-of-days-in-the-month base))]
     (balance-sheet storage-spec entity-id end)))
  ([storage-spec entity-id as-of]
   (->> (accounts/select-nested-by-entity-id
          storage-spec
          entity-id)
        (into [])
        (set-balances-in-account-groups storage-spec as-of)
        transform-balance-sheet)))
