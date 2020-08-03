(ns clj-money.transactions
  (:require [clj-money.accounts :refer [polarize-quantity
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
  (let [account (find-account-fn account-id)
        other-account (find-account-fn other-account-id)]
    (-> transaction
        (assoc :items [(derive-item quantity account)
                       (derive-item (- 0 quantity) other-account)])
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
  (update-in transaction [:items] #(mapv entryfy-item %)))

(defn- unentryfy-item
  [{:keys [debit-quantity credit-quantity] :as item}]
  (-> item
      (assoc :action (if debit-quantity
                       :debit
                       :credit)
             :quantity (or debit-quantity credit-quantity))
      (dissoc :debit-quantity :credit-quantity)))

(defn unentryfy
  "Reverses an entryfy operation"
  [transaction]
  (update-in transaction [:items] #(mapv unentryfy-item %)))
