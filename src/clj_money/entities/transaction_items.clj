(ns clj-money.entities.transaction-items
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.entities :as entities]))

(defn- fetch-account
  [account-or-ref accounts]
  (if (util/entity-ref? account-or-ref)
    (if-let [account (accounts (:id account-or-ref))]
      [account accounts]
      (let [account (entities/find account-or-ref :account)]
        [account (assoc accounts (:id account) account)]))
    [account-or-ref accounts]))

(defn realize-accounts
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

(defn- ->criteria
  [items]
  (let [range (util/->range (map :transaction/transaction-date items)
                            :compare t/before?)
        criterion (if (apply = range)
                    (first range)
                    (apply vector :between range))]
    (util/entity-type
      {:id [:in (map :id items)]
       :transaction/transaction-date criterion}
      :transaction-item)))

(defn resolve-refs
  [items]
  {:pre [(s/valid? :reconciliation/items items)]}
  (let [[full abbr] (split-with :transaction-item/account
                                items)]
    (concat
      full
      (when (seq abbr)
        (entities/select (->criteria abbr))))))
