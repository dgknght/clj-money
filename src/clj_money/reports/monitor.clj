(ns clj-money.reports.monitor
  (:require [java-time.api :as t]
            [clj-money.entities :as entities]
            [clj-money.util :as util :refer [entity=]]
            [clj-money.dates :as dates]
            [clj-money.budgets :as budgets]
            [clj-money.entities.budgets :as bdgs]
            [clj-money.reports :as rpt]))

(defn- monitor-item
  [budget actual percentage]
  #:report{:total-budget budget
           :actual actual
           :percentage percentage
           :prorated-budget (* percentage budget)
           :actual-percent (if (zero? budget)
                             (if (zero? actual) 0M 1M)
                             (/ actual budget))})

(defn- aggregate-account-actuals
  [accounts entity since as-of]
  {:pre [since as-of (not (t/before? as-of since))]}
  (->> accounts
       (rpt/valuate-accounts {:since since
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
               :account (util/->entity-ref account)
               :period (monitor-item period-budget period-actual percent-of-period)
               :budget (monitor-item total-budget total-actual percent-of-total)})))

(defn- aggregate-item
  [{:keys [budget account]}]
  (let [items (bdgs/find-items-by-account (:budget/items budget) account)]
    (when (seq items)
      {:account account
       :periods (->> items
                     (map :budget-item/periods)
                     (apply interleave)
                     (partition (count items))
                     (mapv #(reduce + %)))})))

(defn- account-and-descendants
  [account]
  (entities/select (util/entity-type
                     (util/->entity-ref account)
                     :account)
                   {:include-children? true}))

(defn account-ids
  "Returns the ids of the accounts whose transactions are reported by the
  monitor for the given account: the account and its descendants."
  [account]
  (->> (account-and-descendants account)
       (map :id)
       (into #{(:id account)})))

(defn- append-account-children
  [{:keys [account] :as ctx}]
  (let [children (remove
                  #(entity= %  account)
                  (account-and-descendants account))]
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

(defn report
  "Returns a mini-report for a specified account against a budget period"
  ([account]
   (report account (t/local-date)))
  ([{:as account :account/keys [entity]} as-of]
   {:pre [account as-of]}

   (if-let [budget (bdgs/find-by-date entity as-of)]
     (monitor-from-budget {:account account
                           :as-of as-of
                           :budget (update-in budget
                                              [:budget/entity]
                                              entities/resolve-ref)})
     #:report{:caption (:account/name account)
              :account account
              :message (format "There is no budget for %s" (dates/format-local-date as-of))})))
