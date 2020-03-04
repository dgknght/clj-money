(ns clj-money.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [clj-time.core :as t]
            [clj-money.util :refer [format-number]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-transacted-storage]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.lots :as lots]))

(s/def ::commodity-id (s/nilable integer?))
(s/def ::account-id (s/nilable integer?))
(s/def ::commodity-account-id (s/nilable integer?))
(s/def ::to-account-id integer?)
(s/def ::from-account-id integer?)
(s/def ::inventory-method #{:fifo :lifo})
(s/def ::lt-capital-gains-account-id (s/nilable integer?))
(s/def ::lt-capital-loss-account-id  (s/nilable integer?))
(s/def ::st-capital-gains-account-id (s/nilable integer?))
(s/def ::st-capital-loss-account-id  (s/nilable integer?))
(s/def ::trade-date validation/local-date?)
(s/def ::transfer-date validation/local-date?)
(s/def ::split-date validation/local-date?)
(s/def ::shares decimal?)
(s/def ::value decimal?)
(s/def ::shares-gained decimal?)
(s/def ::purchase (s/keys :req-un [::trade-date
                                   ::shares
                                   ::value]
                          :opt-un [::commodity-id ; TODO: we need wth account-id and commodity-id or commodity-account-id
                                   ::account-id
                                   ::commodity-account-id]))
(s/def ::sale (s/keys :req-un [::trade-date
                               ::shares
                               ::value]
                      :opt-un [::account-id
                               ::commodity-id
                               ::commodity-account-id
                               ::lt-capital-gains-account-id
                               ::lt-capital-loss-account-id
                               ::st-capital-gains-account-id
                               ::st-capital-loss-account-id]))
(s/def ::transfer (s/keys :req-un [::transfer-date
                                   ::shares
                                   ::from-account-id
                                   ::to-account-id
                                   ::commodity-id]))
(s/def ::split (s/keys :req-un [::split-date
                                ::commodity-id
                                ::account-id
                                ::shares-gained]))

(defn- create-price
  "Given a context, calculates and appends the share price"
  [{:keys [storage shares value commodity trade-date] :as context}]
  (assoc context :price (prices/create storage
                                       {:commodity-id (:id commodity)
                                        :trade-date trade-date
                                        :price (with-precision 4 (/ value shares))})))

(defn- acquire-commodity-account
  "Given a context with a commodity-account-id, appends the
  commodity account and the commodity"
  [{:keys [storage commodity-account-id] :as context}]
  (if commodity-account-id
    (let [commodity-account (accounts/find-by-id storage commodity-account-id)
          account (accounts/find-by-id storage (:parent-id commodity-account))
          commodity (commodities/find-by-id storage (:commodity-id commodity-account))]
      (assoc context
             :commodity-account commodity-account
             :account account
             :commodity commodity))
    context))

(defn- acquire-commodity
  "Given a purchase context, appends the commodity"
  [{:keys [commodity-id storage] :as context}]
  (if (:commodity context)
    context
    (assoc context :commodity (commodities/find-by-id storage commodity-id))))

(defn- ensure-tag
  "Appends the :trading tag to the account if it isn't there already"
  [storage tag {:keys [tags] :as account}]
  (if  (and tags
            (tags account) tag)
    account
    (accounts/update storage
                     (update-in account [:tags] conj tag))))

(defn- find-commodity-account
  [storage parent commodity]
  (when-let [result (->> {:parent-id (:id parent)
                          :commodity-id (:id commodity)}
                         (accounts/search storage)
                         first)]
    (ensure-tag storage :tradable result)))

(defn- create-commodity-account
  [storage parent commodity]
  (accounts/create storage {:name (:symbol commodity)
                            :type :asset
                            :commodity-id (:id commodity)
                            :parent-id (:id parent)
                            :entity-id (:entity-id parent)
                            :tags #{:tradable}}))

(defn- find-or-create-commodity-account
  [storage parent commodity]
  (some #(% storage parent commodity)
        [find-commodity-account
         create-commodity-account]))

(defn- acquire-accounts
  "Give a purchase context, acquires the accounts
  necessary to complete the purchase"
  [{:keys [account-id storage commodity]
    :as context}]
  (let [account (or (:account context)
                    (->> account-id
                         (accounts/find-by-id storage)
                         (ensure-tag storage :trading)))
        commodity-account (or (:commodity-account context)
                              (find-or-create-commodity-account storage
                                                                account
                                                                commodity))]
    (assert account (str "Unable to resolve the account " (prn-str (select-keys context [:account :account-id]))))
    (assoc context
           :account account
           :commodity-account commodity-account)))

(defn- acquire-entity
  [{storage :storage
    {entity-id :entity-id} :account
    :as context}]
  (assoc context :entity (entities/find-by-id storage entity-id)))

(defn- sale-transaction-description
  [{:keys [shares]
    {symbol :symbol} :commodity
    {price :price} :price}]
  (format "Sell %s shares of %s at %s"
          shares
          symbol
          (format-number price {:format :commodity-price})))

(defn- purchase-transaction-description
  [{:keys [shares]
    {symbol :symbol} :commodity
    {price :price} :price}]
  (format "Purchase %s shares of %s at %s"
          shares
          symbol
          (format-number price {:format :commodity-price})))

(defn- create-purchase-transaction
  "Given a purchase context, creates the general currency
  transaction"
  [{:keys [storage
           trade-date
           value
           shares
           fee-account-id
           lot]
    :as context}]
  (let [fee (or (:fee context) 0M)
        currency-amount (+ value fee)
        items (cond-> [{:action :credit
                        :account-id (-> context :account :id)
                        :quantity currency-amount
                        :value currency-amount}
                       {:action :debit
                        :account-id (-> context :commodity-account :id)
                        :quantity shares
                        :value value}]
                (not= 0M fee) (conj {:action :debit
                                     :account-id fee-account-id
                                     :quantity fee
                                     :value fee}))]
    (assoc context
           :transaction
           (transactions/create
             storage
             {:entity-id (-> context :account :entity-id)
              :transaction-date trade-date
              :description (purchase-transaction-description context)
              :items items
              :lot-items [{:lot-id (:id lot)
                           :lot-action :buy
                           :price (with-precision 4 (/ value shares))
                           :shares shares}]}))))

(defn- create-capital-gains-item
  [{:keys [quantity description long-term?]} context]
  (let [[action effect] (if (< quantity 0)
                          [:debit "loss"]
                          [:credit "gains"])
        account-key (keyword
                      (format "%s-capital-%s-account-id"
                              (if long-term? "lt" "st")
                              effect))
        account-id (account-key context)]
    {:action action
     :account-id account-id
     :quantity (.abs quantity)
     :value (.abs quantity)
     :memo description}))

(defn- create-capital-gains-items
  [{:keys [gains] :as context}]
  (mapv #(create-capital-gains-item % context) gains))

(defn- create-sale-transaction-items
  [{:keys [shares value] :as context}]
  (let [total-gains (reduce + (map :quantity (:gains context)))
        fee (or (:fee context) 0M)
        items (-> (create-capital-gains-items context)
                  (conj {:action :debit
                         :account-id (-> context :account :id)
                         :quantity (- value fee)
                         :value (- value fee)})
                  (conj {:action :credit
                         :account-id (-> context :commodity-account :id)
                         :quantity shares
                         :value (- value total-gains)}))]
    (cond-> items
      (not= 0M fee) (conj {:action :debit
                           :account-id (:fee-account-id context)
                           :quantity fee
                           :value fee}))))

(defn- create-sale-transaction
  "Given a purchase context, creates the general currency
  transaction"
  [{:keys [storage trade-date] :as context}]
  (let [items (create-sale-transaction-items context)
        transaction (transactions/create
                      storage
                      {:entity-id (-> context :account :entity-id)
                       :transaction-date trade-date
                       :description (sale-transaction-description context)
                       :items items
                       :lot-items (:lot-items context)})]
    (if (validation/has-error? transaction)
      (do
        (log/errorf "Unable to create the commodity sale transaction: %s" transaction)
        (throw (ex-info "Unable to create the commodity sale transaction." {:transaction transaction})))
      (assoc context :transaction transaction))))

(defn- create-lot
  "Given a purchase context, creates and appends the commodity lot"
  [{:keys [storage
           trade-date
           shares
           commodity
           account
           price] :as context}]
  (let [lot (lots/create storage {:account-id (:id account)
                                  :commodity-id (:id commodity)
                                  :purchase-date trade-date
                                  :purchase-price (:price price)
                                  :shares-purchased shares})]
    (assoc context :lot lot)))

(def ^:private purchase-coercion-rules
  [(coercion/rule :local-date [:trade-date])
   (coercion/rule :integer [:account-id])
   (coercion/rule :integer [:commodity-id])
   (coercion/rule :integer [:commodity-account-id])
   (coercion/rule :decimal [:shares])
   (coercion/rule :decimal [:value])])

(defn- validate-purchase
  [purchase]
  (-> purchase
      (coercion/coerce purchase-coercion-rules)
      (validation/validate ::purchase)))

; expect
; either
;   :commodity-id
;   :account-id
; or
;   :commodity-account-id
; :trade-date
; :shares
; :value
(defn buy
  [storage-spec purchase]
  (with-transacted-storage [s storage-spec]
    (let [validated (validate-purchase purchase)]
      (if (validation/valid? validated)
        (->> (assoc validated :storage s)
             acquire-commodity-account
             acquire-commodity
             acquire-accounts
             acquire-entity
             create-price
             create-lot
             create-purchase-transaction)
        validated))))

(defn unbuy
  "Reverses a commodity purchase"
  [storage-spec {transaction-id :id transaction-date :transaction-date}]
  (with-transacted-storage [s storage-spec]
    (let [transaction (transactions/find-by-id s transaction-id transaction-date)
          lot (lots/find-by-id s (-> transaction :lot-items first :lot-id))
          commodity (commodities/find-by-id s (:commodity-id lot))]
      (when (not= (:shares-purchased lot) (:shares-owned lot))
        (throw (IllegalStateException.
                 "Cannot undo a purchase if shares have been sold from the lot")))
      (transactions/delete s transaction-id transaction-date)
      (lots/delete s (:id lot))
      {:transaction transaction
       :lot lot
       :commodity commodity})))

(defn- acquire-lots
  "Given a sell context, finds the next lot containing
  shares that can be sold"
  [{:keys [storage inventory-method commodity account] :as context}]
  (assoc context
         :lots (lots/search storage
                            {:commodity-id (:id commodity)
                             :account-id (:id account)
                             :shares-owned [:!= 0M]}
                            {:sort [[:purchase-date
                                     (if (= :lifo inventory-method)
                                       :desc
                                       :asc)]]})))

(defn- process-lot-sale
  [context lot shares-to-sell]
  (let [shares-owned (:shares-owned lot)
        [shares-sold
         remaining-shares-to-sell
         new-lot-balance] (if (>= shares-owned shares-to-sell)
                            [shares-to-sell
                             0
                             (- shares-owned shares-to-sell)]
                            [shares-owned
                             (- shares-to-sell shares-owned)
                             0])
        sale-price (-> context :price :price)
        purchase-price (:purchase-price lot)
        adj-lot (lots/update (:storage context)
                             (assoc lot :shares-owned new-lot-balance))
        gain (- (* shares-sold sale-price)
                (* shares-sold purchase-price))
        cut-off-date (t/plus (:purchase-date lot) (t/years 1))
        long-term? (>= 0 (compare cut-off-date
                                  (:trade-date context)))]
    [(-> context
         (update-in [:lot-items] #(conj % {:lot-id (:id adj-lot)
                                           :lot-action :sell
                                           :shares shares-sold
                                           :price sale-price}))
         (update-in [:updated-lots] #(conj % adj-lot))
         (update-in [:gains] #(conj % {:description (format "Sell %s shares of %s at %s"
                                                            shares-sold
                                                            (-> context :commodity :symbol)
                                                            (format-number sale-price {:format :commodity-price}) )
                                       :quantity gain
                                       :long-term? long-term?})))
     remaining-shares-to-sell]))

(defn- process-lot-sales
  "Given a sell context, processes the lot changes and appends
  the new lot transactions and the affected lots"
  [{:keys [lots] :as context}]
  (loop [context (assoc context :lots []
                                :gains []
                                :lot-items [])
         shares-remaining (:shares context)
         lot (first lots)
         remaining-lots (rest lots)]
    (if lot
      (let [[adj-context
             shares-to-be-sold] (process-lot-sale context
                                                  lot
                                                  shares-remaining)]
        (if (= 0 shares-to-be-sold)
          adj-context
          (recur adj-context shares-to-be-sold (first remaining-lots)  (rest remaining-lots))))
      (do
        (log/error "Unable to find a lot to sell shares " (prn-str (dissoc context :storage)))
        (throw (ex-info "Unable to find a lot to sell the shares"
                        {:context (dissoc context :storage)}))))))

(def ^:private sale-coercion-rules
  (concat purchase-coercion-rules
          [(coercion/rule :integer [:lt-capital-gains-account-id])
           (coercion/rule :integer [:st-capital-gains-account-id])
           (coercion/rule :integer [:lt-capital-loss-account-id])
           (coercion/rule :integer [:st-capital-loss-account-id])
           (coercion/rule :keyword [:inventory-method])]))

(defn- validate-sale
  [_ sale]
  (-> sale
      (coercion/coerce sale-coercion-rules)
      (validation/validate ::sale)))

(defn- update-entity-settings
  [{:keys [entity storage] :as context}]
  (entities/update storage
                   (update-in entity
                              [:settings]
                              #(merge % (select-keys context
                                                     [:lt-capital-gains-account-id
                                                      :st-capital-gains-account-id
                                                      :lt-capital-loss-account-id
                                                      :st-capital-loss-account-id
                                                      :inventory-method]))))
  context)

(defn- find-or-create-account
  [storage account]
  (or (accounts/find-by storage account)
      (accounts/create storage account)))

(defn- ensure-gains-account
  [{:keys [entity storage] :as context} [term result]]
  (let [k (keyword (str term "-capital-" result "-account-id"))
        n (str (if (= "lt" term) "Long-term" "Short-term")
               " Capital "
               (if (= "gains" result) "Gains" "Losses"))]
    (cond
      (k context)
      context

      (k (:settings entity))
      (assoc context k (k (:settings entity)))

      :else
      (let [account (find-or-create-account
                      storage
                      {:entity-id (:id entity)
                       :type (if (= "gains" result)
                               :income
                               :expense)
                       :name n})]
        (assoc context k (:id account))))))

(defn- ensure-gains-accounts
  "Ensures that the gain/loss accounts are present
  in the sale transaction."
  [context]
  (->> (for [term ["lt" "st"]
             result ["gains" "loss"]]
         [term result])
       (reduce ensure-gains-account context)))

(defn sell
  [storage-spec sale]
  (with-transacted-storage [s storage-spec]
    (let [validated (validate-sale s sale)]
      (if (validation/has-error? validated)
        validated
        (->> (assoc validated :storage s)
             acquire-commodity-account
             acquire-commodity
             acquire-accounts
             acquire-entity
             acquire-lots
             ensure-gains-accounts
             update-entity-settings
             create-price
             process-lot-sales
             create-sale-transaction)))))

(defn unsell
  [storage-spec {transaction-id :id transaction-date :transaction-date}]
  (with-transacted-storage [s storage-spec]
    (let [transaction (transactions/find-by-id s transaction-id transaction-date)]
      (doseq [lot-item (:lot-items transaction)]
        (let [lot (lots/find-by-id s (:lot-id lot-item))]
          (lots/update s (update-in lot [:shares-owned] #(+ % (:shares lot-item))))))
      (transactions/delete s transaction-id transaction-date))))

(def ^:private transfer-coercion-rules
  [(coercion/rule :local-date [:transfer-date])
   (coercion/rule :integer [:from-account-id])
   (coercion/rule :integer [:to-account-id])
   (coercion/rule :integer [:commodity-id])
   (coercion/rule :decimal [:shares])])

(defn- validate-transfer
  [transfer]
  (-> transfer
      (coercion/coerce transfer-coercion-rules)
      (validation/validate ::transfer)))

(defn- append-commodity
  [{:keys [storage commodity-id] :as context}]
  (assoc context :commodity (commodities/find-by-id storage commodity-id)))

(defn- append-transfer-accounts
  [{:keys  [storage from-account-id to-account-id commodity] :as context}]
  (let [[from-account
         to-account] (->> [from-account-id to-account-id]
                          (map #(accounts/find-by-id storage %))
                          (map #(ensure-tag storage :trading %)))
        [from-commodity-account
         to-commodity-account] (map #(find-or-create-commodity-account
                                       storage
                                       %
                                       commodity)
                                    [from-account to-account])]
    (assoc context
           :from-account from-account
           :from-commodity-account from-commodity-account
           :to-account to-account
           :to-commodity-account to-commodity-account)))

(defn- process-transfer-lots
  [{:keys [storage commodity from-account to-account] :as context}]
  (let [to-move (lots/search storage {:commodity-id (:id commodity)
                                      :account-id (:id from-account)
                                      :shares-owned [:> 0M]})]
    (log/warnf "No lots found to transfer %s" (prn-str context))
    (assoc context :lots (mapv #(lots/update
                                  storage
                                  (assoc % :account-id (:id to-account)))
                               to-move))))

(defn- create-transfer-transaction
  [{:keys [commodity
           from-commodity-account
           to-commodity-account
           transfer-date
           shares]
    s :storage
    :as context}]
  (let [price (prices/most-recent s (:id commodity) transfer-date)
        value (* shares (:price price))
        transaction (transactions/create s {:entity-id (:entity-id commodity)
                                            :transaction-date transfer-date
                                            :description (format "Transfer %s shares of %s"
                                                                 shares
                                                                 (:symbol commodity))
                                            :items [{:action :credit
                                                     :quantity shares
                                                     :value value
                                                     :account-id (:id from-commodity-account)}
                                                    {:action :debit
                                                     :quantity shares
                                                     :value value
                                                     :account-id (:id to-commodity-account)}]})]
    (assoc context :transaction transaction)))

(defn transfer
  [storage-spec transfer]
  (let [validated (validate-transfer transfer)]
    (if (validation/valid? validated)
      (with-transacted-storage [s storage-spec]
        (-> validated
            (assoc :storage s)
            append-commodity
            append-transfer-accounts
            process-transfer-lots
            create-transfer-transaction
            (select-keys [:lots :transaction])))
      validated)))

(defn- append-commodity-account
  [{:keys [storage commodity account-id] :as context}]
  (assoc context
         :commodity-account
         (accounts/find-by storage {:commodity-id (:id commodity)
                                    :entity-id (:entity-id commodity)
                                    :parent-id account-id})))

(defn- apply-split-to-lot
  [storage ratio lot]
  (let [updated (-> lot
                    (update-in [:shares-purchased] #(* % ratio))
                    (update-in [:shares-owned] #(* % ratio))
                    (update-in [:purchase-price] #(with-precision 4 (/ % ratio))))]
    (lots/update storage updated)))

(defn- append-split-lots
  [{:keys [storage commodity-id account-id] :as context}]
  (assoc context :lots (lots/search storage {:commodity-id commodity-id
                                             :account-id account-id
                                             :shares-owned [:!= 0M]})))

(defn- append-split-ratio
  [{:keys [shares-gained lots] :as context}]
  (if (empty? lots)
    (throw (ex-info "Not lots found to which to apply the split."
                    (select-keys context [:commodity-id :account-id])))
    (let [shares-owned (->> lots
                            (map :shares-owned)
                            (reduce + 0M))]
      (assoc context :ratio (with-precision 4
                              (/ (+ shares-owned shares-gained)
                                 shares-owned))))))

(defn- process-split-lots
  [{:keys [storage lots ratio] :as context}]
  (assoc context
         :lots
          (mapv #(apply-split-to-lot storage ratio %) lots)))

(defn- ratio->words
  [ratio]
  ; We'll need to expand this at some point to handle
  ; reverse splits and stranger splits, like 3:2
  (let [[n d] (cond->> [ratio 1]
                (< ratio 1)
                (map (comp int
                           #(/ % ratio))))]
    (format "%s for %s" n d)))

(defn- create-split-transaction
  [{:keys [storage
           commodity
           split-date
           ratio
           commodity-account
           shares-gained] :as context}]
  (assoc context
         :transaction
         (transactions/create storage
                              {:entity-id (:entity-id commodity)
                               :transaction-date split-date
                               :description (format "Split shares of %s %s"
                                                    (:symbol commodity)
                                                    (ratio->words ratio))
                               :items [{:action (if (< 0 shares-gained)
                                                  :debit
                                                  :credit)
                                        :account-id (:id commodity-account)
                                        :quantity (.abs shares-gained)
                                        :value 0M}]})))

(defn- validate-split
  [split]
  (validation/validate split ::split))

(defn split
  "Records a stock split

  :commodity-id  - identifies the commodity being split
  :split-date    - the date the split is effective
  :shares-gained - the difference in the number of shares held before and after the split
  :account-id    - the trading account through which the commodity was purchased"

  [storage-spec split]
  (let [validated (validate-split split)]
    (if (validation/valid? validated)
      (with-transacted-storage [s storage-spec]
        (-> (assoc validated :storage s)
            append-commodity
            append-commodity-account
            append-split-lots
            append-split-ratio
            process-split-lots
            create-split-transaction
            (dissoc :storage))) validated)))
