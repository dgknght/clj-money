(ns clj-money.transactions
  (:require [clojure.set :refer [rename-keys]]
            [clojure.pprint :refer [pprint]]
            [clj-money.util :as util :refer [->model-ref model=]]
            [clj-money.dates :as dates]
            [clj-money.decimal :as d]
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
  {:pre [(can-simplify? trx)
         ref-account]}
  (let [{[{:transaction-item/keys [quantity action] :as account-item}] true
         [other-item] false} (group-by #(model= ref-account
                                                (:transaction-item/account %))
                                       items)]
    (-> trx
        (assoc :transaction/other-account (:transaction-item/account other-item)
               :transaction/other-item (->model-ref other-item)
               :transaction/account (:transaction-item/account account-item)
               :transaction/item (->model-ref account-item)
               :transaction/quantity (polarize-quantity quantity action ref-account))
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
  [{:transaction-item/keys [quantity action] :as item}]
  (-> item
      (assoc :transaction-item/debit-quantity (when (= :debit action)
                               quantity)
             :transaction-item/credit-quantity (when (= :credit action)
                                quantity))
      (dissoc :transaction-item/action
              :transaction-item/quantity)))

(defn entryfy
  "Accepts a standard transaction and returns the transaction
  with line items adjusted for easy entry, with no :action
  attribute, one :credit-quantity and one :debit-quantity"
  [transaction]
  (update-in transaction [:transaction/items] #(conj (mapv entryfy-item %)
                                                     {})))

(def ^:private has-quantity?
  (some-fn :transaction-item/debit-quantity
            :transaction-item/credit-quantity))

(def ^:private empty-item?
  (complement has-quantity?))

(defn- unentryfy-item
  [{:transaction-item/keys [debit-quantity credit-quantity] :as item}]
  (let [quantity (or debit-quantity credit-quantity)]
    (-> item
        (assoc :transaction-item/action (if debit-quantity
                                          :debit
                                          :credit)
               :transaction-item/quantity quantity
               :transaction-item/value quantity)
        (dissoc :transaction-item/debit-quantity
                :transaction-item/credit-quantity))))

(defn unentryfy
  "Reverses an entryfy operation"
  [trx]
  (update-in trx
             [:transaction/items]
             #(->> %
                   (remove empty-item?)
                   (mapv unentryfy-item))))

(defn ensure-empty-item
  "Given an entryfied transaction, ensures that there is
  exactly one empty row"
  [transaction]
  (update-in transaction [:transaction/items] #(conj (vec (remove empty? %)) {})))

(defn tradify
  [{:transaction/keys [items] :as trx} {:keys [find-account find-commodity]}]
  (let [{:keys [trading tradable]} (->> items
                                        (map #(update-in % [:transaction-item/account] find-account))
                                        (mapcat (fn [item]
                                                  (map #(vector % item)
                                                       (-> item
                                                           :transaction-item/account
                                                           :account/system-tags))))
                                        (into {}))]
    (-> trx
        (rename-keys {:transaction/transaction-date :trade/trade-date})
        (assoc :trade/account (:transaction-item/account trading)
               :trade/commodity (when tradable
                                  (-> tradable
                                      :transaction-item/account
                                      :account/commodity
                                      find-commodity))
               :trade/shares (some :transaction-item/quantity [tradable trading])
               :trade/action (if (= :credit (:transaction-item/action trading))
                               :buy
                               :sell))
        (dissoc :transaction/items))))

(defn untradify
  [{:trade/keys [shares action account commodity] :as trade}
   {:keys [find-account-with-commodity]}]
  (let [tradable (find-account-with-commodity commodity)
        items (->> [:credit :debit]
                   (interleave (if (= :buy action)
                                 [account tradable]
                                 [tradable account]))
                   (partition 2)
                   (map #(hash-map :transaction-item/account (first %)
                                   :transaction-item/action (second %)
                                   :transaction-item/quantity shares)))]
    (-> trade
        (rename-keys {:trade/trade-date :transaction/transaction-date})
        (assoc :transaction/items items)
        (dissoc :trade/account :trade/shares :trade/action :trade/commodity))))

(defn polarize-item-quantity
  [{:transaction-item/keys [account quantity action polarized-quantity] :as item}]
  (if polarized-quantity
    item
    (assoc item
         :transaction-item/polarized-quantity
         (polarize-quantity quantity action account))))

(defn- summarize-period
  [[start-date end-date] items]
  {:start-date start-date
   :end-date end-date
   :quantity (->> items
                  (filter #(dates/within? (:transaction-item/transaction-date %) start-date end-date))
                  (map (comp :transaction-item/polarized-quantity
                             polarize-item-quantity))
                  (reduce d/+ 0M))})

(defn summarize-items
  [{:keys [interval-type interval-count since as-of]
    :or {interval-count 1}
    :as opts}
   items]
  {:pre [(:as-of opts)]}
  (->> (dates/ranges since
                     (dates/period interval-type interval-count)
                     :inclusive true)
       (take-while #(apply dates/overlaps? since as-of %))
       (map #(summarize-period % items))))

(defn expand
  "Given a transaction with a quantity, debit account and credit acount, return
  a transaction with two items, one for each account"
  [{:as trx :transaction/keys [debit-account credit-account quantity]}]
  (if (:transaction/items trx)
    trx
    (-> trx
        (assoc :transaction/items [#:transaction-item{:action :debit
                                                      :quantity quantity
                                                      :account debit-account}
                                   #:transaction-item{:action :credit
                                                      :quantity quantity
                                                      :account credit-account}])
        (dissoc :transaction/quantity
                :transaction/debit-account
                :transaction/credit-account))))

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
