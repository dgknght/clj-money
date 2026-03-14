(ns clj-money.entities.transaction-items
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.util :as util]
            [clj-money.accounts :refer [polarize-quantity]]
            [clj-money.entities :as entities]))

(defn realize-accounts
  "Given a list of transaction items, lookup referenced accounts"
  [items]
  (when-let [account-ids (->> items
                              (map (comp :id :transaction-item/account))
                              set
                              seq)]
    (let [accounts (index-by :id
                             (entities/find-many account-ids))]
      (map #(update-in %
                       [:transaction-item/account]
                       (comp accounts :id))
           items))))

(defn polarize-quantities
  [items]
  (->> items
       realize-accounts
       (map #(assoc %
                    :transaction-item/polarized-quantity
                    (polarize-quantity {:account (:transaction-item/account %)
                                        :action (:transaction-item/action %)
                                        :quantity (:transaction-item/quantity %)})))))

(defn resolve-refs
  [items]
  {:pre [(s/valid? :reconciliation/items items)]}
  (let [[full abbr] (split-with :account-item/account
                                items)]
    (concat
      full
      (when (seq abbr)
        (entities/select
          (util/entity-type
            {:id [:in (map :id abbr)]}
            :account-item))))))
