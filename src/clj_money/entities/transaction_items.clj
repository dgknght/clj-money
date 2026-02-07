(ns clj-money.entities.transaction-items
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]))

(defn realize-accounts
  "Given a list of account items, lookup referenced accounts"
  [items]
  (when-let [account-ids (->> items
                              (map (comp :id :account-item/account))
                              set
                              seq)]
    (let [accounts (index-by :id
                             (entities/find-many account-ids))]
      (map #(update-in %
                       [:account-item/account]
                       (comp accounts :id))
           items))))

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
