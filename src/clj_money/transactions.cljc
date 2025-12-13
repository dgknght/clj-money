(ns clj-money.transactions
  (:require [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.util :as util :refer [->entity-ref entity=]]
            [clj-money.dates :as dates]
            [clj-money.decimal :as d]
            [clj-money.accounts :refer [polarize-quantity
                                        ->transaction-item]]))

(def ^:private val-or-qty
  (some-fn :transaction-item/value
           :transaction-item/quantity))

; A simple transaction involves two accounts and one quantity
; and is represented by a single map
; E.g.
; {:transaction/transaction-date "2020-01-01"
;  :transaction/description "Coffee"
;  :transaction/quantity 10M
;  :transaction/debit-account {:id :credit-card}
;  :transaction/credit-account {:id :dining}}

; A bilateral transaction involves any number of transaction items
;, each of which having a quantity, a debit account and a credit account
; This is the way the transaction is stored
; E.g.
; {:transaction/transaction-date "2020-01-01"
;  :transaction/description "Coffee"
;  :transaction/items [{:transaction-item/quantity 5M
;                       :transaction-item/debit-account {:id :credit-card}
;                       :transaction-item/credit-account {:id :dining}}
;                      {:transaction-item/quantity 5M
;                       :transaction-item/debit-account {:id :credit-card}
;                       :transaction-item/credit-account {:id :gifts}
;                       :transaction-item/memo "Gift card for holiday party"}]}
 
; A unilateral transaction involves any number of transaction items
;, which of which having a quantity, an account, and an action
; E.g.
; {:transaction/transaction-date "2020-01-01"
;  :transaction/description "Coffee"
;  :transaction/items [{:transaction-item/quantity 10M
;                       :transaction-item/action :debit
;                       :transaction-item/account {:id :credit-card}}
;                      {:transaction-item/quantity 5M
;                       :transaction-item/action :credit
;                       :transaction-item/account {:id :dining}}
;                      {:transaction-item/quantity 5M
;                       :transaction-item/action :credit
;                       :transaction-item/account {:id :gifts}
;                       :transaction-item/memo "Gift card for holiday party"}]}

(defn sum-of-credits-equals-sum-of-debits?
  [{:transaction/keys [items]}]
  (if (= 1 (count items))
    (zero? (:transaction-item/value (first items))) ; a split transaction will have one item with a value of zero
    (let [{:keys [debit credit]}
          (->> items
               (map second)
               (group-by :transaction-item/action)
               (map #(update-in % [1] (fn [itms]
                                        (->> itms
                                             (map val-or-qty)
                                             (reduce + 0M)))))
               (into {}))]
      (= debit credit))))

(s/def ::id (some-fn integer?
                     string?))
(s/def ::entity-ref (s/keys :req-un [::id]))

(s/def :transaction-item/quantity (s/and d/decimal? pos?))
(s/def :transaction-item/account ::entity-ref)
(s/def :transaction-item/action #{:debit :credit})
(s/def :transaction-item/debit-account ::entity-ref)
(s/def :transaction-item/credit-account ::entity-ref)
(s/def :transaction-item/memo (s/nilable string?))
(s/def :transaction/entity ::entity-ref)
(s/def :transaction/transaction-date dates/local-date?)
(s/def :transaction/debit-account ::entity-ref)
(s/def :transaction/credit-account ::entity-ref)
(s/def :transaction/items (s/coll-of ::transaction-item :min-count 1))
(s/def :transaction/memo (s/nilable string?))

(s/def ::common-transaction (s/keys :req [:transaction/entity
                                          :transaction/description
                                          :transaction/transaction-date]
                                    :opt [:transaction/memo]))

(s/def ::simple-transaction (s/merge ::common-transaction
                                     (s/keys :req [:transaction/quantity
                                                   :transaction/debit-account
                                                   :transaction/credit-account])))

(s/def ::unilateral-item (s/keys :req [:transaction-item/account
                                       :transaction-item/action
                                       :transaction-item/quantity]
                                 :opt [:transaction-item/memo]))
; I believe the min-count would be 2 except for split trading transactions
(s/def ::unilateral-items (s/coll-of ::unilateral-item :min-count 1))

(s/def ::bilateral-item (s/keys :req [:transaction-item/credit-account
                                      :transaction-item/debit-account
                                      :transaction-item/quantity]
                                :opt [:transaction-item/memo]))
(s/def ::bilateral-items (s/coll-of ::bilateral-item :min-count 1))

(s/def ::transaction-item (s/or :unilateral ::unilateral-item
                                :bilateral ::bilateral-item))

(s/def :transaction/items (s/coll-of ::transaction-item))

(s/def ::complex-transaction (s/merge ::common-transaction
                                       (s/keys :req [:transaction/items])))

(defn- just-maps
  [items]
  (map #(if (vector? %)
          (second %)
          %)
       items))

; When using s/conform, each item
; will have been conformed, so each entry
; in the list will be a tuple with
; the type of item in the 1st position
; and the item in the 2nd
(defn- valid-items?
  [spec items]
  (->> items
       just-maps
       (s/valid? spec)))

(s/def ::bilateral-transaction (s/and ::complex-transaction
                                      (fn [{:transaction/keys [items]}]
                                        (valid-items? ::bilateral-items items))))

(s/def ::unilateral-transaction (s/and ::complex-transaction
                                       (fn [{:transaction/keys [items]}]
                                         (valid-items? ::unilateral-items items))
                                       sum-of-credits-equals-sum-of-debits?))

(s/def ::transaction (s/or :simple ::simple-transaction
                           :unilateral ::unilateral-transaction
                           :bilateral ::bilateral-transaction))

(defn accountified?
  "Returns true if the accountify fn has been applied to the transaction."
  [{:transaction/keys [account]}]
  (not (not account)))

(defn can-accountify?
  "Returns true if the transaction can be simplified (which
  means it has two items) or false if not (which means it
  has more). It assumes a valid transaction."
  [{:transaction/keys [items]}]
  (= 2
     (count (remove empty? items))))

(defn accountify
  "Accepts a standard transaction with two line items and
  returns a simplified transaction vis a vis the specified
  account, with the amount polarized.

  If the transaction contains more or less than two items, an
  exception is thrown."
  [{:transaction/keys [items] :as trx} ref-account]
  {:pre [(can-accountify? trx)
         ref-account]}
  (let [{[{:transaction-item/keys [quantity action] :as account-item}] true
         [other-item] false} (group-by #(entity= ref-account
                                                (:transaction-item/account %))
                                       items)]
    (-> trx
        (assoc :transaction/other-account (:transaction-item/account other-item)
               :transaction/other-item (->entity-ref other-item)
               :transaction/account (:transaction-item/account account-item)
               :transaction/item (->entity-ref account-item)
               :transaction/quantity (polarize-quantity quantity action ref-account))
        (dissoc :transaction/items))))

(defn unaccountify
  "Accepts a simplified transaction (with one quantity, one debit
  account, and one credit account) and returns a standard
  transaction (with line items)"
  [{:transaction/keys [quantity account other-account item other-item] :as trx} find-account]
  {:pre [(:transaction/account trx)]}
  (let [item-1 (merge item
                      (->transaction-item
                        {:quantity quantity
                         :account (find-account account)}))
        item-2 (when other-account
                 #:transaction-item{:quantity (d/abs quantity)
                                    :action (if (= :credit (:transaction-item/action item-1))
                                              :debit
                                              :credit)
                                    :account other-account})]
    (cond-> (-> trx
                (dissoc :transaction/quantity
                        :transaction/account
                        :transaction/other-account
                        :transaction/item
                        :transaction/other-item)
                (assoc :transaction/items [item-1]))
      item-2 (update-in [:transaction/items] conj (merge other-item item-2)))))

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
                  (filter #(dates/within? (:transaction/transaction-date %) start-date end-date))
                  (map (comp :transaction-item/polarized-quantity
                             polarize-item-quantity))
                  (reduce d/+ 0M))})

(defn summarize-items
  [{:keys [period since as-of]
    :or {period [1 :month]}
    :as opts}
   items]
  {:pre [(:as-of opts)]}
  (->> (dates/ranges since
                     (dates/period period)
                     :inclusive true)
       (take-while (fn [[start _]]
                     (t/before? start as-of)))
       (map #(summarize-period % items))))

(defn expand
  "Given a transaction with a quantity, debit account and credit acount, return
  a transaction with two items, one for each account"
  [{:as trx :transaction/keys [debit-account credit-account quantity]}]
  (if (and debit-account credit-account quantity)
    (-> trx
        (assoc :transaction/items [#:transaction-item{:action :debit
                                                      :quantity quantity
                                                      :account debit-account}
                                   #:transaction-item{:action :credit
                                                      :quantity quantity
                                                      :account credit-account}])
        (dissoc :transaction/quantity
                :transaction/debit-account
                :transaction/credit-account))
    trx))

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

(defn- simple->bilateral
  [{:transaction/keys [debit-account credit-account quantity] :as trx}]
  (-> trx
      (dissoc :transaction/debit-account
              :transaction/credit-account
              :transaction/quantity)
      (assoc :transaction/items [{:transaction-item/quantity quantity
                                  :transaction-item/debit-account debit-account
                                  :transaction-item/credit-account credit-account}])))

(defn- bilateral->simple
  [{[[_ {:transaction-item/keys [debit-account
                                 credit-account
                                 quantity]}]]
    :transaction/items
    :as trx}]
  ; The transaction has been conformed, so the items collection
  ; is a list of tuples like [:item-type item]
  (-> trx
      (dissoc :transaction/items)
      (assoc :transaction/debit-account debit-account
             :transaction/credit-account credit-account
             :transaction/quantity quantity)))

(defn- group-items-by-action
  [items]
  (->> items
       (map second)
       (group-by :transaction-item/action)))

(defn- merge-sides
  [out debits credits]
  (let [d (first debits)
        c (->> credits
               (filter #(= (val-or-qty d)
                           (val-or-qty %)))
               first)]

    (when-not c
      (throw (ex-info "Imbalanced transaction" {:debits debits
                                                :credits credits})))

    [(conj out (-> d
                   (dissoc :transaction-item/action)
                   (rename-keys {:transaction-item/account
                                 :transaction-item/debit-account})
                   (assoc :transaction-item/credit-account (:transaction-item/account c))))
     (rest debits)
     (remove #(= c %)
             credits)]))

(defn- unilateral->bilateral-items
  [items]
  (let [grouped (group-items-by-action items)]
    (loop [out []
           debits (:debit grouped)
           credits (:credit grouped)]
      (cond
        (and (empty? debits)
             (empty? credits))
        out

        (or (empty? debits)
            (empty? credits))
        (throw (ex-info "Imbalanced transaction" {:items items}))

        :else
        (let [[o d c] (merge-sides out debits credits)]
          (recur o d c))))))

(defn- unilateral->bilateral
  [trx]
  (update-in trx
             [:transaction/items]
             unilateral->bilateral-items))

(defn- conform-trx
  [trx]
  (let [conformed (s/conform ::transaction trx)]
    (when (vector? conformed)
      conformed)))

(defn ->bilateral
  [input]
  (let [[type trx] (conform-trx input)]
    (case type
      :simple (simple->bilateral trx)
      :unilateral (unilateral->bilateral trx)
      nil)))

(defn simplify
  [input]
  (let [[type trx] (conform-trx input)]
    (case type
      :bilateral (when (= 1 (count (:transaction/items trx)))
                   (bilateral->simple trx))
      nil)))
