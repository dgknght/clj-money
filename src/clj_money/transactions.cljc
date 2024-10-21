(ns clj-money.transactions
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [clj-money.util :as util :refer [->model-ref model=]]
            [clj-money.dates :as dates]
            [clj-money.accounts :refer [polarize-quantity
                                        ->transaction-item]]))

(defn can-simplify?
  "Returns true if the transaction can be simplified (which
  means it has two items) or false if not (which means it
  has more). It assumes a valid transaction."
  [{:transaction/keys [items]}]
  (= 2 (count items)))

(defn accountify
  "Accepts a standard transaction with two line items and
  returns a simplified transaction vis a vis the specified
  account, with the amount polarized.

  If the transaction contains more or less than two items, an
  exception is thrown."
  [{:transaction/keys [items] :as trx} ref-account]
  {:pre [(can-simplify? trx)]}
  (let [{[account-item] true
         [other-item] false} (group-by #(model= ref-account
                                                (:transaction-item/account %))
                                       items)]
    (-> trx
        (assoc :transaction/other-account (:transaction-item/account other-item)
               :transaction/other-item (->model-ref other-item)
               :transaction/account (:transaction-item/account account-item)
               :transaction/item (->model-ref account-item)
               :transaction/quantity (polarize-quantity account-item ref-account))
        (dissoc :transaction/items))))

(defn unaccountify
  "Accepts a simplified transaction (with one quantity, one debit
  account, and one credit account) and returns a standard
  transaction (with line items)"
  [{:transaction/keys [quantity account other-account item other-item] :as trx} find-account]
  {:pre [(:transaction/account trx)
         (:transaction/other-account trx)
         (:transaction/item trx)
         (:transaction/other-item trx)]}
  (let [item-1 (merge item (->transaction-item quantity
                                               (find-account account)))]
    (-> trx
        (assoc :transaction/items
               [item-1
                (merge other-item
                       #:transaction-item{:quantity (util/abs quantity)
                                          :action (if (= :credit
                                                         (:transaction-item/action item-1))
                                                    :debit
                                                    :credit)
                                          :account other-account})])
        (dissoc :transaction/quantity
                :transaction/account
                :transaction/other-account
                :transaction/item
                :transaction/other-item))))

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
                                                       (:system-tags (:account item)))))
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
                                   :quantity shares)))]
    (-> transaction
        (rename-keys {:trade-date :transaction-date})
        (assoc :items items)
        (dissoc :account-id :shares :action :commodity-id))))

(defn- summarize-period
  [[start-date end-date] items]
  {:start-date start-date
   :end-date end-date
   :quantity (->> items
                  (filter #(dates/within? (:transaction-date %) start-date end-date))
                  (map :polarized-quantity)
                  (reduce + 0M))})

(defn summarize-items
  [{:keys [interval-type interval-count start-date end-date]
    :or {interval-count 1}}
   items]
  (->> (dates/ranges start-date
                     (dates/period interval-type interval-count)
                     :inclusive true)
       (take-while #(apply dates/overlaps? start-date end-date %))
       (map #(summarize-period % items))))

(defn change-date
  [trx new-date]
  (-> trx
      (rename-keys {:transaction-date :original-transaction-date})
      (assoc :transaction-date new-date)))

(defn expand
  "Given a transaction with a quantity, debit account and credit acount, return
  a transaction with two items, one for each account"
  [{:as trx :transaction/keys [debit-account credit-account quantity]}]
  (if (:items trx)
    trx
    (-> trx
        (assoc :transaction/items [#:transaction-item{:action :debit
                                                      :quantity quantity
                                                      :account debit-account}
                                   #:transaction-item{:action :credit
                                                      :quantity quantity
                                                      :account credit-account}])
        (dissoc :transaction/quantity
                :transaction/debit-account-id
                :transaction/credit-account-id))))

(defn value
  [{:transaction/keys [items]}]
  (let [sums (->> items
                  (filter :transaction-item/value)
                  (group-by :transaction-item/action)
                  (map (fn [[_ items]]
                         (->> items
                              (map :transaction-item/value)
                              (reduce + 0M))))
                  set)]
    (when (= 1 (count sums))
      (first sums))))
