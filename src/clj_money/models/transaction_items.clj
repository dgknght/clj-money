(ns clj-money.models.transaction-items
  (:require [clj-money.util :as util]
            [clj-money.models :as models]))

(defn- fetch-account
  [account-or-ref accounts]
  (if (util/model-ref? account-or-ref)
    (if-let [account (accounts (:id account-or-ref))]
      [account accounts]
      (let [account (models/find account-or-ref :account)]
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
