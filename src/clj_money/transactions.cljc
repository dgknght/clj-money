(ns clj-money.transactions
  (:require [clojure.set :refer [rename-keys
                                 intersection
                                 union
                                 difference]]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [dgknght.app-lib.core :refer [update-in-if
                                          index-by]]
            [clj-money.util :as util :refer [->entity-ref entity=]]
            [clj-money.dates :as dates]
            [clj-money.decimal :as d]
            [clj-money.accounts :refer [polarize-quantity
                                        ->transaction-item]]))

(def ^:private val-or-qty
  (some-fn :transaction-item/value
           :transaction-item/quantity))

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

(s/def ::id (complement nil?))
(s/def ::entity-ref (s/keys :req-un [::id]))

(s/def :account-item/account ::entity-ref)
(s/def :account-item/quantity d/decimal?)
(s/def :account-item/action #{:debit :credit})
(s/def :account-item/memo (s/nilable string?))
(s/def ::account-item (s/keys :req [:account-item/account
                                    :account-item/quantity
                                    :account-item/action]
                              :opt [:account-item/memo]))

(s/def :transaction-item/quantity (s/and d/decimal? pos?))
(s/def :transaction-item/value (s/and d/decimal? pos?))
(s/def :transaction-item/debit-quantity (s/nilable (s/and d/decimal? pos?)))
(s/def :transaction-item/credit-quantity (s/nilable (s/and d/decimal? pos?)))
(s/def :transaction-item/account ::entity-ref)
(s/def :transaction-item/action #{:debit :credit})
(s/def :transaction-item/debit-item ::account-item)
(s/def :transaction-item/credit-item ::account-item)
(s/def :transaction-item/memo (s/nilable string?))
(s/def :transaction-item/debit-memo (s/nilable string?))
(s/def :transaction-item/credit-memo (s/nilable string?))
(s/def :transaction/entity ::entity-ref)
(s/def :transaction/transaction-date dates/local-date?)
(s/def :transaction/debit-account ::entity-ref)
(s/def :transaction/credit-account ::entity-ref)
(s/def :transaction/items (s/coll-of :clj-money.entities/transaction-item :min-count 1))
(s/def :transaction/memo (s/nilable string?))
(s/def :transaction/description string?)

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

(s/def ::bilateral-item (s/keys :req [:transaction-item/credit-item
                                      :transaction-item/debit-item
                                      :transaction-item/value]))
(s/def ::bilateral-items (s/coll-of ::bilateral-item :min-count 1))

(s/def :clj-money.entities/transaction-item
  (s/or :unilateral ::unilateral-item
        :bilateral ::bilateral-item))

(s/def :transaction/items (s/coll-of :clj-money.entities/transaction-item
                                     :min-count 1))

(s/def ::complex-transaction (s/merge ::common-transaction
                                      (s/keys :req [:transaction/items])))

(defn- just-map
  [x]
  (if (vector? x)
    (second x)
    x))

; When using s/conform, each item
; will have been conformed, so each entry
; in the list will be a tuple with
; the type of item in the 1st position
; and the item in the 2nd
(defn- valid-items?
  [spec items]
  (if (seq items)
    (->> items
         (map just-map)
         (s/valid? spec))
    true))

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
  "Accepts an accountified transaction (with one quantity, one account, and one
   'other' account) and returns a unilateral transaction"
  [{:transaction/keys [quantity
                       account
                       other-account
                       item
                       other-item]
    :as trx}
   find-account]
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
      (assoc :transaction/items
             [{:transaction-item/value quantity
               :transaction-item/debit-item
               {:account-item/action :debit
                :account-item/account debit-account
                :account-item/quantity (polarize-quantity
                                         {:quantity quantity
                                          :account debit-account
                                          :action :debit})}
               :transaction-item/credit-item
               {:account-item/action :credit
                :account-item/account credit-account
                :account-item/quantity (polarize-quantity
                                         {:quantity quantity
                                          :account credit-account
                                          :action :credit})}}])))

(defn- simple->unilateral
  [{:transaction/keys [debit-account credit-account quantity] :as trx}]
  (-> trx
      (dissoc :transaction/debit-account
              :transaction/credit-account
              :transaction/quantity)
      (assoc :transaction/items [{:transaction-item/quantity quantity
                                  :transaction-item/value quantity
                                  :transaction-item/action :debit
                                  :transaction-item/account debit-account}
                                 {:transaction-item/quantity quantity
                                  :transaction-item/value quantity
                                  :transaction-item/action :credit
                                  :transaction-item/account credit-account}])))

(defn- bilateral->simple
  [{[{:transaction-item/keys [debit-item
                              credit-item
                              value]}]
    :transaction/items
    :as trx}]
  ; The transaction has been conformed, so the items collection
  ; is a list of tuples like [:item-type item]
  (-> trx
      (dissoc :transaction/items)
      (assoc :transaction/debit-account (:account-item/account debit-item)
             :transaction/credit-account (:account-item/account credit-item)
             :transaction/quantity value)))

(defn- group-items-by-action
  [items]
  (update-vals
    (group-by :transaction-item/action items)
    (partial sort-by val-or-qty >)))

(def ^:private transaction->account-item-mappings
  {:transaction-item/quantity :account-item/quantity
   :transaction-item/memo     :account-item/memo
   :transaction-item/action   :account-item/action
   :transaction-item/index    :account-item/index
   :transaction-item/balance  :account-item/balance
   :transaction-item/account  :account-item/account})

(defn- transaction->account-item
  [{:as item :transaction-item/keys [account action]}
   & {:keys [omit-memo]}]
  (let [mappings (cond-> transaction->account-item-mappings
                   omit-memo (dissoc :transaction-item/memo))]
    (-> item
        (rename-keys mappings)
        (select-keys (vals mappings))
        (update-in [:account-item/quantity] #(polarize-quantity
                                               {:account account
                                                :quantity %
                                                :action action})))))

(defn- d+c
  "Combine two unilateral items of the same value into one bilateral item"
  [d c]
  (let [item-value (val-or-qty d)
        ids (intersection (:ids d) (:ids c))
        memos (->> [d c]
                   (map :transaction-item/memo)
                   (set))
        memo (when (= 1 (count memos))
               (first memos))]
    (when (< 1 (count ids))
      (throw (ex-info "Unmatched item ids" {:debit d
                                            :credit c})))
    (cond->
      {:transaction-item/value item-value
       :transaction-item/debit-item
       (transaction->account-item d :omit-memo memo)
       :transaction-item/credit-item
       (transaction->account-item c :omit-memo memo)}
      memo
      (assoc :transaction-item/memo (first memos))
      (seq ids)
      (assoc :id (first ids)))))

(defn- merge-equals
  [[d & debits] [c & credits]]
  [(d+c d c)
   debits
   credits])

(defn- merge-partial-credit
  [[d & debits] [c & credits]]
  (let [d-val (val-or-qty d)
        c-val (val-or-qty c)
        d-qty (:transaction-item/quantity d)
        sliced-c (-> c
                     (update-in [:ids] intersection (:ids d))
                     (assoc :transaction-item/quantity d-qty
                            :transaction-item/value d-val))
        remaining-c (-> c
                        (update-in [:ids] difference (:ids d))
                        (update-in [:transaction-item/quantity] - d-qty)
                        (update-in [:transaction-item/value] (fnil - c-val) d-val))]
    [(d+c d sliced-c)
     debits
     (->> credits
          (cons remaining-c)
          (sort-by val-or-qty >))]))

(defn- merge-partial-debit
  [[d & debits] [c & credits]]
  (let [d-val (val-or-qty d)
        c-val (val-or-qty c)
        c-qty (:transaction-item/quantity c)
        sliced-d (-> d
                     (update-in [:ids] intersection (:ids c))
                     (assoc :transaction-item/quantity c-qty
                            :transaction-item/value c-val))
        remaining-d (-> d
                        (update-in-if [:ids] difference (:ids c))
                        (update-in [:transaction-item/quantity] - c-qty)
                        (update-in [:transaction-item/value] (fnil - d-val) c-val))]
    [(d+c sliced-d c)
     (->> debits
          (cons remaining-d)
          (sort-by val-or-qty >))
     credits]))

(defn- pluck-matches
  [out debits credits]
  (let [grouped (->> credits
                     (concat debits)
                     (filter :transaction-item/memo)
                     (reduce
                       (fn [r i]
                         (update-in
                           r
                           ((juxt (juxt val-or-qty
                                        :transaction-item/memo)
                                  :transaction-item/action)
                            i)
                           (fnil conj [])
                           i))
                       {})
                     (filter #(< 1 (count (val %)))))]
    (when (seq grouped)
      (let [[_ {[d] :debit [c] :credit}] (first grouped)]
        [(conj out (d+c d c))
         (remove #(= d %) debits)
         (remove #(= c %) credits)]))))

(defn- apply-slices
  [out debits credits]
  (let [d (val-or-qty (first debits))
        c (val-or-qty (first credits))
        [item ds cs] (cond
                       (= d c)
                       (merge-equals debits credits)

                       (< d c)
                       (merge-partial-credit debits credits)

                       :else
                       (merge-partial-debit debits credits))]
    [(conj out item)
     ds
     cs]))

(defn- merge-sides
  [out debits credits]
  (or (pluck-matches out debits credits)
      (apply-slices out debits credits)))

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

(defn- account->transaction-item
  [item]
  (-> item
      (rename-keys {:account-item/memo :transaction-item/memo
                    :account-item/account :transaction-item/account
                    :account-item/action :transaction-item/action
                    :account-item/quantity :transaction-item/quantity})
      (select-keys [:transaction-item/memo
                    :transaction-item/action
                    :transaction-item/quantity
                    :transaction-item/account])
      (update-in [:transaction-item/quantity] d/abs)))

(defn- split-item
  "Takes a bilateral item and returns two unilateral items"
  [{:transaction-item/keys [debit-item
                            credit-item
                            value
                            memo]
    :keys [id]}]
  [(cond->
     (-> debit-item
         account->transaction-item
         (assoc :transaction-item/value value))
     id (assoc :ids #{id})
     memo (assoc :transaction-item/memo memo))
   (cond->
     (-> credit-item
         account->transaction-item
         (assoc :transaction-item/value value))
     id (assoc :ids #{id})
     memo (assoc :transaction-item/memo memo))])

(defn- consolidate-items
  [items]
  (->> items
       (group-by (juxt (comp :id
                             :transaction-item/account)
                       :transaction-item/memo))
       (map (fn [[_ [i & is]]]
              (-> i
                  (update-in-if [:ids] #(apply union % (map :ids is)))
                  (update-in [:transaction-item/quantity]
                             +
                             (->> is
                                  (map :transaction-item/quantity)
                                  (reduce +)))
                  (update-in [:transaction-item/value]
                             +
                             (->> is
                                  (map :transaction-item/value)
                                  (reduce +))))))))

(defn- bilateral->unilateral-items
  [items]
  (->> items
       (mapcat split-item)
       consolidate-items))

(defn- unilateral->bilateral
  [trx]
  (update-in trx
             [:transaction/items]
             unilateral->bilateral-items))

(defn- bilateral->unilateral
  [trx]
  (update-in trx
             [:transaction/items]
             bilateral->unilateral-items))

(defn- conform-trx
  [trx]
  (let [conformed (s/conform ::transaction trx)]
    (when (vector? conformed)
      conformed)))

(defn ->bilateral
  [input]
  (let [[type] (conform-trx input)]
    (case type
      :simple (simple->bilateral input)
      :unilateral (unilateral->bilateral input)
      :bilateral input
      nil)))

(defn ->unilateral
  [input]
  (let [[type] (conform-trx input)]
    (case type
      :simple (simple->unilateral input)
      :bilateral (bilateral->unilateral input)
      :unilateral input
      nil)))

(defn simplify
  [input]
  (let [[type] (conform-trx input)]
    (case type
      :bilateral (when (= 1 (count (:transaction/items input)))
                   (bilateral->simple input))
      nil)))

(defn- account-item
  [action account value]
  #:account-item{:action action
                 :account account
                 :quantity (polarize-quantity
                             {:account account
                              :quantity value
                              :action action})})

(defn expand-account-item
  [{:transaction-item/keys [value
                            credit-account
                            credit-item
                            debit-account
                            debit-item]
    :as item}]
  (cond-> (dissoc item
                  :transaction-item/credit-account
                  :transaction-item/debit-account)
    credit-account
    (assoc :transaction-item/credit-item
           (account-item :credit credit-account value))

    debit-account
    (assoc :transaction-item/debit-item
           (account-item :debit debit-account value))

    credit-item
    (update-in [:transaction-item/credit-item]
               #(merge (account-item :credit
                                     (:account-item/account %)
                                     value)))

    debit-item
    (update-in [:transaction-item/debit-item]
               #(merge (account-item :debit
                                     (:account-item/account %)
                                     value)))))
