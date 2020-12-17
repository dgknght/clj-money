(ns clj-money.transactions
  (:require [clojure.set :refer [rename-keys]]
            [clj-money.util :as util]
            [clj-money.accounts :refer [polarize-quantity
                                        derive-item]]))

(defn can-simplify?
  "Returns true if the transaction can be simplified (which
  means it has two items) or false if not (which means it
  has more). It assumes a valid transaction."
  [{:keys [items]}]
  (= 2 (count items)))

(defn simplify
  "Accepts a standard transaction (with line items) and
  returns a simplified transaction (with one quantity, one
  debit account and one credit account). Note that the
  transaction must have only two items."
  [{:keys [items] :as transaction} ref-account]
  {:pre [(can-simplify? transaction)]}
  (let [[account-item
         other-item] (sort-by #(if (= (:id ref-account)
                                      (:account-id %))
                                 0
                                 1)
                              items)]
    (-> transaction
        (assoc :other-account-id (:account-id other-item)
               :account-id (:account-id account-item)
               :quantity (polarize-quantity account-item ref-account))
        (dissoc :items))))

(defn fullify
  "Accepts a simplified transaction (with one quantity, one debit
  account, and one credit account) and returns a standard
  transaction (with line items)"
  [{:keys [quantity account-id other-account-id] :as transaction} find-account-fn]
  {:pre [(:account-id transaction)]}
  (let [account (find-account-fn account-id)
        other-account (find-account-fn other-account-id)
        item-1 (derive-item quantity account)]
    (-> transaction
        (assoc :items [item-1
                       {:quantity (util/abs quantity)
                        :action (if (= :credit
                                       (:action item-1))
                                  :debit
                                  :credit)
                        :account-id (:id other-account)}])
        (dissoc :quantity :account-id :other-account-id))))

(defn- entryfy-item
  [{:keys [quantity action] :as item}]
  (-> item
      (assoc :debit-quantity (when (= :debit action)
                               quantity)
             :credit-quantity (when (= :credit action)
                                quantity))
      (dissoc :action :quantity)))

(defn entryfy
  "Accepts a standard transaction and returns the transaction
  with line items adjusted for easy entry, with no :action
  attribute, one :credit-quantity and one :debit-quantity"
  [transaction]
  (update-in transaction [:items] #(conj (mapv entryfy-item %)
                                         {})))

(def ^:private empty-item?
  (complement
    (some-fn :debit-quantity
             :credit-quantity)))

(defn- unentryfy-item
  [{:keys [debit-quantity credit-quantity] :as item}]
  (let [quantity (or debit-quantity credit-quantity)]
    (-> item
        (assoc :action (if debit-quantity
                         :debit
                         :credit)
               :quantity quantity
               :value quantity)
        (dissoc :debit-quantity :credit-quantity))))

(defn unentryfy
  "Reverses an entryfy operation"
  [transaction]
  (update-in transaction [:items] #(->> %
                                        (remove empty-item?)
                                        (mapv unentryfy-item))))

(defn ensure-empty-item
  "Given an entryfied transaction, ensures that there is
  exactly one empty row"
  [transaction]
  (update-in transaction [:items] #(conj (vec (remove empty? %)) {})))

(defn tradify
  [{:keys [items] :as transaction} {:keys [find-account find-commodity]}]
  (let [{:keys [trading tradable]} (->> items
                                        (map #(assoc % :account (find-account (:account-id %))))
                                        (mapcat (fn [item]
                                                  (map #(vector % item)
                                                       (:tags (:account item)))))
                                        (into {}))]
    (-> transaction
        (rename-keys {:transaction-date :trade-date})
        (assoc :account-id (:account-id trading)
               :commodity-id (when tradable
                               (-> tradable
                                   :account
                                   :commodity-id
                                   find-commodity
                                   :id))
               :shares (some :quantity [tradable trading])
               :action (if (= :credit (:action trading))
                         :buy
                         :sell))
        (dissoc :items))))

(defn untradify
  [{:keys [shares action account-id] :as transaction}
   {:keys [find-account-by-commodity-id]}]
  (let [tradable-id (-> transaction
                        :commodity-id
                        find-account-by-commodity-id
                        :id)
        items (->> [:credit :debit]
                   (interleave (if (= :buy action)
                                 [account-id tradable-id]
                                 [tradable-id account-id]))
                   (partition 2)
                   (map #(hash-map :account-id (first %)
                                   :action (second %)
                                   :quantity shares))) ]
    (-> transaction
        (rename-keys {:trade-date :transaction-date})
        (assoc :items items)
        (dissoc :account-id :shares :action :commodity-id))))
