(ns clj-money.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [config.core :refer [env]]
            [clj-time.core :as t]
            [stowaway.implicit :refer [with-transacted-storage]]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.lots :as lots]
            [clj-money.models.lot-transactions :as lot-trans])
  (:import java.math.BigDecimal))

(s/def ::commodity-id (s/nilable integer?))
(s/def ::account-id (s/nilable integer?))
(s/def ::commodity-account-id (s/nilable integer?))
(s/def ::to-account-id integer?)
(s/def ::inventory-method #{:fifo :lifo})
(s/def ::lt-capital-gains-account-id (s/nilable integer?))
(s/def ::lt-capital-loss-account-id  (s/nilable integer?))
(s/def ::st-capital-gains-account-id (s/nilable integer?))
(s/def ::st-capital-loss-account-id  (s/nilable integer?))
(s/def ::trade-date v/local-date?)
(s/def ::transfer-date v/local-date?)
(s/def ::split-date v/local-date?)
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
                                   ::to-account-id
                                   ::commodity-id]))
(s/def ::split (s/keys :req-un [::split-date
                                ::commodity-id
                                ::account-id
                                ::shares-gained]))

(defn- create-price
  "Given a context, calculates and appends the share price"
  [{:keys [shares value commodity trade-date] :as context}]
  (assoc context :price (prices/create {:commodity-id (:id commodity)
                                        :trade-date trade-date
                                        :price (with-precision 4 (/ value shares))}
                                       {:skip-account-update? true})))

(defn- ensure-tag
  "Appends the :trading tag to the account if it isn't there already"
  [{:keys [system-tags] :as account} tag]
  (when account
    (if (contains? system-tags tag)
      account
      (accounts/update (update-in account [:system-tags] conj tag)))))

(defn- append-commodity-account
  "If the argument contains a commodity-account-id, append
  that account and :account-id and :commodity-id to the map
  and return it."
  [{:keys [commodity-account-id] :as context}]
  (if commodity-account-id
    (let [{:keys [parent-id commodity-id]
           :as account} (ensure-tag
                         (accounts/find commodity-account-id)
                         :tradable)]
      (assert account (str "Unable to load the commodity account: " commodity-account-id))
      (assoc context
             :commodity-account account
             :account-id parent-id
             :commodity-id commodity-id))
    context))

(defn- append-commodity
  "Given a context, appends the commodity"
  [{:keys [commodity-id] :as context}]
  {:pre [commodity-id]}

  (assoc context :commodity (commodities/find commodity-id)))

(defn- find-commodity-account
  [parent commodity]
  (when-let [result (accounts/find-by {:parent-id (:id parent)
                                       :commodity-id (:id commodity)})]
    (ensure-tag result :tradable)))

(defn- create-commodity-account
  [parent commodity]
  (accounts/create {:name (:symbol commodity)
                    :type :asset
                    :commodity-id (:id commodity)
                    :parent-id (:id parent)
                    :entity-id (:entity-id parent)
                    :system-tags #{:tradable}}))

(defn- find-or-create-commodity-account
  [parent commodity]
  (some #(% parent commodity)
        [find-commodity-account
         create-commodity-account]))

(defn- append-accounts
  "Give a purchase context, acquires the accounts
  necessary to complete the purchase"
  [{:keys [account-id commodity]
    :as context}]
  {:pre [(and account-id commodity)]}
  (let [account (ensure-tag (accounts/find account-id)
                            :trading)
        commodity-account (find-or-create-commodity-account account
                                                            commodity)]
    (assert account (str "Unable to resolve the account "
                         (prn-str (select-keys context [:account-id]))))
    (assert commodity-account (str "Unable to resolve the commodity account"
                                   (prn-str (select-keys context [:account-id :commodity]))))
    (assoc context
           :account account
           :commodity-account commodity-account)))

(defn- append-entity
  [{{entity-id :entity-id} :account
    :as context}]
  (assoc context :entity (entities/find entity-id)))

(defn- sale-transaction-description
  [{:keys [shares]
    {symbol :symbol} :commodity
    {price :price} :price}]
  (format "Sell %s shares of %s at %s"
          shares
          symbol
          (format-decimal price {:fraction-digits 3})))

(defn- purchase-transaction-description
  [{:keys [shares]
    {symbol :symbol} :commodity
    {price :price} :price}]
  (format "Purchase %s shares of %s at %s"
          shares
          symbol
          (format-decimal price {:fraction-digits 3})))

(defn- create-purchase-transaction
  "Given a purchase context, creates the general currency
  transaction"
  [{:keys [trade-date
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
  (let [total-gains (->> (:gains context)
                         (map :quantity)
                         (reduce +))
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
      (not (zero? fee)) (conj {:action :debit
                               :account-id (:fee-account-id context)
                               :quantity fee
                               :value fee}))))

(defn- create-sale-transaction
  "Given a purchase context, creates the general currency
  transaction"
  [{:keys [trade-date] :as context}]
  (let [items (create-sale-transaction-items context)
        transaction (transactions/create
                     {:entity-id (-> context :account :entity-id)
                      :transaction-date trade-date
                      :description (sale-transaction-description context)
                      :items items
                      :lot-items (:lot-items context)})]
    (if (v/has-error? transaction)
      (do
        (log/errorf "Unable to create the commodity sale transaction: %s" transaction)
        (throw (ex-info "Unable to create the commodity sale transaction." {:transaction transaction})))
      (assoc context :transaction transaction))))

(defn- create-lot
  "Given a purchase context, creates and appends the commodity lot"
  [{:keys [trade-date
           shares
           commodity
           account
           price] :as context}]
  (let [lot (lots/create {:account-id (:id account)
                          :commodity-id (:id commodity)
                          :purchase-date trade-date
                          :purchase-price (:price price)
                          :shares-purchased shares})]
    (assoc context :lot lot)))

(defn- update-account-meta
  "Given a successful trade that creates a new price,
  update any accounts with values affected by the new price."
  [{:keys [price] :as trade}]
  (prices/update-accounts price)
  trade)

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
  [purchase]
  (with-transacted-storage (env :db)
    (with-validation purchase ::purchase []
      (->> purchase
           append-commodity-account
           append-commodity
           append-accounts
           append-entity
           create-price
           create-lot
           create-purchase-transaction
           update-account-meta))))

(defn unbuy
  "Reverses a commodity purchase"
  [{transaction-id :id transaction-date :transaction-date}]
  (with-transacted-storage (env :db)
    (let [transaction (transactions/find transaction-id transaction-date)
          lot (lots/find (-> transaction :lot-items first :lot-id))
          commodity (commodities/find (:commodity-id lot))]
      (when (not= (:shares-purchased lot) (:shares-owned lot))
        (throw (IllegalStateException.
                "Cannot undo a purchase if shares have been sold from the lot")))
      (transactions/delete transaction)
      (lots/delete lot)
      {:transaction transaction
       :lot lot
       :commodity commodity})))

(defn- acquire-lots
  "Given a sell context, finds the next lot containing
  shares that can be sold"
  [{:keys [inventory-method commodity account] :as context}]
  (assoc context
         :lots (lots/search {:commodity-id (:id commodity)
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
                             0M
                             (- shares-owned shares-to-sell)]
                            [shares-owned
                             (- shares-to-sell shares-owned)
                             0M])
        sale-price (-> context :price :price)
        purchase-price (:purchase-price lot)
        adj-lot (lots/update (assoc lot :shares-owned new-lot-balance))
        gain (.setScale
              (- (* shares-sold sale-price)
                 (* shares-sold purchase-price))
              2
              BigDecimal/ROUND_HALF_UP)
        cut-off-date (t/plus (:purchase-date lot) (t/years 1))
        long-term? (>= 0 (compare cut-off-date
                                  (:trade-date context)))]
    (when (v/has-error? adj-lot)
      (log/errorf "Unable to update lot for sale %s" adj-lot))
    [(-> context
         (update-in [:lot-items] #(conj % {:lot-id (:id adj-lot)
                                           :lot-action :sell
                                           :shares shares-sold
                                           :price sale-price}))
         (update-in [:updated-lots] #(conj % adj-lot))
         (update-in [:gains] #(conj % {:description (format "Sell %s shares of %s at %s"
                                                            shares-sold
                                                            (-> context :commodity :symbol)
                                                            (format-decimal sale-price {:fraction-digits 3}))
                                       :quantity gain
                                       :long-term? long-term?})))
     remaining-shares-to-sell]))

(defn- process-lot-sales
  "Given a sell context, processes the lot changes and appends
  the new lot transactions and the affected lots"
  [{:keys [lots shares] :as context}]

  (when (> shares (->> lots
                       (map :shares-owned)
                       (reduce + 0M)))
    (log/warnf "Attempt to sell more shares when owned: %s" context))

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
        (if (zero? shares-to-be-sold)
          adj-context
          (recur adj-context shares-to-be-sold (first remaining-lots)  (rest remaining-lots))))
      (do
        (log/error "Unable to find a lot to sell shares " (prn-str context))
        (throw (ex-info "Unable to find a lot to sell the shares"
                        {:context context}))))))

(defn- update-entity-settings
  [{:keys [entity] :as context}]
  (entities/update (update-in entity
                              [:settings]
                              #(merge % (select-keys context
                                                     [:lt-capital-gains-account-id
                                                      :st-capital-gains-account-id
                                                      :lt-capital-loss-account-id
                                                      :st-capital-loss-account-id
                                                      :inventory-method]))))
  context)

(defn- find-or-create-account
  [account]
  (some #(% account)
        [accounts/find-by
         accounts/create]))

(defn- find-or-create-gains-account
  [{:keys [entity]} term result]
  (find-or-create-account
   {:entity-id (:id entity)
    :type (if (= "gains" result)
            :income
            :expense)
    :name (str (if (= "lt" term) "Long-term" "Short-term")
               " Capital "
               (if (= "gains" result) "Gains" "Losses"))}))

(defn- ensure-gains-account
  [{:keys [entity] :as context} [term result]]
  (let [k (keyword (str term "-capital-" result "-account-id"))]
    (if (k context)
      context
      (assoc context k (or (k (:settings entity))
                           (:id (find-or-create-gains-account context
                                                              term
                                                              result)))))))

(defn- ensure-gains-accounts
  "Ensures that the gain/loss accounts are present
  in the sale transaction."
  [context]
  (->> (for [term ["lt" "st"]
             result ["gains" "loss"]]
         [term result])
       (reduce ensure-gains-account context)))

(defn sell
  [sale]
  (with-transacted-storage (env :db)
    (with-validation sale ::sale []
      (->> sale
           append-commodity-account
           append-commodity
           append-accounts
           append-entity
           acquire-lots
           ensure-gains-accounts
           update-entity-settings
           create-price
           process-lot-sales
           create-sale-transaction
           update-account-meta))))

(defn unsell
  [{transaction-id :id transaction-date :transaction-date}]
  (with-transacted-storage (env :db)
    (let [transaction (transactions/find transaction-id transaction-date)]
      (doseq [lot-item (:lot-items transaction)]
        (let [lot (lots/find (:lot-id lot-item))]
          (lots/update (update-in lot [:shares-owned] #(+ % (:shares lot-item))))))
      (transactions/delete transaction))))

(defn- append-transfer-accounts
  [{:keys  [from-account from-account-id to-account to-account-id commodity] :as context}]
  (let [to-account (ensure-tag (or to-account
                                   (accounts/find to-account-id))
                               :trading)
        from-account (ensure-tag (or from-account
                                     (accounts/find from-account-id))
                                 :trading)
        [from-commodity-account
         to-commodity-account] (map #(find-or-create-commodity-account
                                      %
                                      commodity)
                                    [from-account to-account])]
    (assoc context
           :from-account from-account
           :from-commodity-account from-commodity-account
           :to-account to-account
           :to-commodity-account to-commodity-account)))

(defn- process-transfer-lots
  [{:keys [commodity from-account to-account shares] :as context}]
  (let [to-move (->> (lots/search {:commodity-id (:id commodity)
                                   :account-id (:id from-account)
                                   :shares-owned [:> 0M]}
                                  {:sort [:purchase-date]})
                     (reduce (fn [acc {:keys [shares-owned] :as lot}]
                               (if (>= (:shares acc) shares)
                                 (reduced acc)
                                 (-> acc
                                     (update-in [:lots] conj lot)
                                     (update-in [:shares] + shares-owned))))
                             {:shares 0M
                              :lots []})
                     :lots)]
    (if (empty? to-move)
      (log/warnf "No lots found to transfer %s" (prn-str context))
      (assoc context :lots (mapv #(lots/update
                                   (assoc % :account-id (:id to-account)))
                                 to-move)))))

(defn- create-transfer-transaction
  [{:keys [commodity
           from-commodity-account
           to-commodity-account
           transfer-date
           shares]
    :as context}]
  (let [price (prices/most-recent commodity transfer-date)
        value (* shares (:price price))
        transaction (transactions/create {:entity-id (:entity-id commodity)
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
  "Transfers a commodity from one account to another

  :commodity-id     - identifies the commodity to be moved
  :shares           - the number of shares of the commodity to be transfered
  :transaction-date - the date on which the transfer takes place
  :from-account-id  - identifies the account from which the commodity is to be moved
  :from-account     - the account from which the commodity is to be moved. Supplying this instead of :from-account-id bypasses the database lookup for the account.
  :to-account-id    - identifies the account to which the commodity is to be moved
  :to-account       - the account to which the commodity is to be moved. Supplying this instead of :to-account-id bypasses the database lookup for the account."
  [transfer]
  (let [validated (v/validate transfer ::transfer)]
    (if (v/valid? validated)
      (with-transacted-storage (env :db)
        (some-> validated
                append-commodity
                append-transfer-accounts
                process-transfer-lots
                create-transfer-transaction
                (select-keys [:lots :transaction])))
      validated)))

; TODO: Simplify this by updating stowaway to allow operators in mass update
(defn- apply-split-to-lot
  [ratio lot]
  (doseq [trx (lot-trans/search {:lot-id (:id lot)})]
    (lot-trans/update (-> trx
                          (update-in [:price] #(with-precision 4 (/ % ratio)))
                          (update-in [:shares] #(* % ratio)))))
  (lots/update (-> lot
                   (update-in [:shares-purchased] #(* % ratio))
                   (update-in [:shares-owned] #(* % ratio))
                   (update-in [:purchase-price] #(with-precision 4 (/ % ratio))))))

(defn- append-split-lots
  [{:keys [commodity-id account-id] :as context}]
  (assoc context :lots (lots/search {:commodity-id commodity-id
                                     :account-id account-id
                                     :shares-owned [:!= 0M]})))

(defn- append-split-ratio
  [{:keys [shares-gained lots] :as context}]
  (assert (seq lots) "No lots found to which to apply the split.")
  (let [shares-owned (->> lots
                          (map :shares-owned)
                          (reduce + 0M))]
    (assoc context :ratio (with-precision 4
                            (/ (+ shares-owned shares-gained)
                               shares-owned)))))

(defn- process-split-lots
  [{:keys [lots ratio] :as context}]
  (assoc context
         :lots
         (mapv #(apply-split-to-lot ratio %) lots)))

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
  [{:keys [commodity
           split-date
           ratio
           commodity-account
           shares-gained] :as context}]
  (assoc context
         :transaction
         (transactions/create {:entity-id (:entity-id commodity)
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
  (v/validate split ::split))

(defn split
  "Records a stock split

  :commodity-id  - identifies the commodity being split
  :split-date    - the date the split is effective
  :shares-gained - the difference in the number of shares held before and after the split
  :account-id    - the trading account through which the commodity was purchased"

  [split]
  (let [validated (validate-split split)]
    (if (v/valid? validated)
      (with-transacted-storage (env :db)
        (-> validated
            append-commodity
            append-accounts
            append-split-lots
            append-split-ratio
            process-split-lots
            create-split-transaction))
      validated)))
