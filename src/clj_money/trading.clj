(ns clj-money.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.validation :as v :refer [with-ex-validation]]
            [clj-money.accounts :refer [system-tagged?]]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.util :as util]
            [clj-money.db :as db])
  (:import java.math.BigDecimal))

(s/def :trade/commodity ::models/model-ref)
(s/def :trade/account ::models/model-ref)
(s/def :trade/commodity-account ::models/model-ref)
(s/def :trade/inventory-method #{:fifo :lifo})
(s/def :trade/lt-capital-gains-account ::models/model-ref)
(s/def :trade/lt-capital-loss-account ::models/model-ref)
(s/def :trade/st-capital-gains-account ::models/model-ref)
(s/def :trade/st-capital-loss-account ::models/model-ref)
(s/def :trade/date t/local-date?)
(s/def :trade/fee decimal?)
(s/def :trade/fee-account ::models/model-ref)
(s/def :trade/shares decimal?)
(s/def :trade/value decimal?)
(s/def ::transfer-date t/local-date?)
(s/def ::split-date t/local-date?)
(s/def :trade/to-account ::models/model-ref)
(s/def ::shares-gained decimal?)

(defmulti ^:private purchase-spec
  (fn [purchase]
    (if (:trade/commodity-account purchase)
      :combined
      :separate)))

(defmethod purchase-spec :combined [_]
  (s/keys :req [:trade/date
                :trade/shares
                :trade/value
                :trade/commodity-account]
          :opt [:trade/fee
                :trade/fee-account]))

(defmethod purchase-spec :separate [_]
  (s/keys :req [:trade/date
                :trade/shares
                :trade/value
                :trade/commodity
                :trade/account]
          :opt [:trade/fee
                :trade/fee-account]))

(s/def ::models/purchase (s/multi-spec purchase-spec :trade/commodity-account))

(defmulti ^:private sale-spec
  (fn [sale]
    (if (:trade/commodity-account sale)
      :combined
      :separate)))

(defmethod sale-spec :combined [_]
  (s/keys :req [:trade/date
                :trade/shares
                :trade/value
                :trade/commodity-account]
          :opt [:trade/inventory-method
                :trade/lt-capital-gains-account
                :trade/lt-capital-loss-account
                :trade/st-capital-gains-account
                :trade/st-capital-loss-account
                :trade/fee
                :trade/fee-account]))

(defmethod sale-spec :separate [_]
  (s/keys :req [:trade/date
                :trade/shares
                :trade/value
                :trade/commodity
                :trade/account]
          :opt [:trade/inventory-method
                :trade/lt-capital-gains-account
                :trade/lt-capital-loss-account
                :trade/st-capital-gains-account
                :trade/st-capital-loss-account
                :trade/fee
                :trade/fee-account]))

(s/def ::models/sale (s/multi-spec sale-spec :trade/commodity-account))

(s/def ::transfer (s/keys :req-un [::transfer-date
                                   ::shares
                                   ::to-account
                                   ::commodity]))
(s/def ::split (s/keys :req-un [::split-date
                                ::commodity
                                ::account
                                ::shares-gained]))

(defn- create-price
  "Given a trade map, calculates and appends the share price"
  [{:trade/keys [shares value commodity date] :as trade}]
  (-> trade
      (update-in [:trade/commodity-account :account/price-as-of]
                 #(dates/earliest % date))
      (assoc :trade/price
             #:price{:commodity commodity
                     :trade-date date
                     :price (with-precision 4 (/ value shares))})))

(defn- ensure-tag
  "Appends the specified tag to the account if it isn't there already"
  [account tag]
  (when account
    (update-in account [:account/system-tags] (fnil conj #{}) tag)))

(defn- append-commodity-account
  "If the argument contains a commodity-account, ensure it is a full model map
  and add the account and commodity refs to the result."
  [{:trade/keys [commodity-account] :as trade}]
  (if commodity-account
    (let [{:account/keys [parent commodity]
           :as account} (models/find commodity-account :account)]
      (assert account (format "Unable to load the commodity account: %s" commodity-account))
      (assoc trade
             :trade/commodity-account account
             :trade/account parent
             :trade/commodity commodity))
    trade))

(defn- append-commodity
  "Given a trade map, appends the commodity"
  [{:trade/keys [commodity] :as trade}]
  {:pre [commodity]}

  (cond-> trade
    (util/model-ref? commodity)
    (assoc :commodity (models/find commodity :commodity))))

(defn- find-commodity-account
  [parent commodity]
  (when-let [result (models/find-by #:account{:parent parent
                                              :commodity commodity})]
    (ensure-tag result :tradable)))

(defn- create-commodity-account
  [parent commodity]
  {:id (util/temp-id)
   :account/name (:commodity/symbol commodity)
   :account/type :asset
   :account/commodity commodity
   :account/parent parent
   :account/entity (:account/entity parent)
   :account/system-tags #{:tradable}})

(defn- find-or-create-commodity-account
  [parent commodity]
  (some #(% parent commodity)
        [find-commodity-account
         create-commodity-account]))

(defn- append-accounts
  "Give a trade map, acquires the accounts
  necessary to complete the purchase"
  [{:trade/keys [account commodity commodity-account]
    :as trade}]
  {:pre [(and account commodity)]}
  (cond-> trade
    (util/model-ref? account)
    (assoc :account (models/find account :account))

    (nil? commodity-account)
    (assoc :trade/commodity-account (find-or-create-commodity-account account commodity))))

(defn- update-accounts
  [{:as trade :trade/keys [price]}]

  ; Note that :account/commodity-price is not saved, but is used in
  ; transactions in order to calculate the value of the account
  (-> trade
      (update-in [:trade/account] ensure-tag :trading)
      (update-in [:trade/commodity-account] ensure-tag :tradable)
      (assoc-in [:trade/commodity-account :account/commodity-price] (:price/price price))))

(defn- append-entity
  [{{:account/keys [entity]} :trade/account
    :as trade}]
  (assoc trade :trade/entity (if (util/model-ref? entity)
                                 (models/find entity :entity)
                                 entity)))

(defn- sale-transaction-description
  [{:trade/keys [shares]
    {:commodity/keys [symbol]} :trade/commodity
    {:price/keys [price]} :trade/price}]
  (format "Sell %s shares of %s at %s"
          shares
          symbol
          (format-decimal price {:fraction-digits 3})))

(defn- purchase-transaction-description
  [{:trade/keys [shares]
    {:commodity/keys [symbol]} :trade/commodity
    {:price/keys [price]} :trade/price}]
  (format "Purchase %s shares of %s at %s"
          shares
          symbol
          (format-decimal price {:fraction-digits 3})))

(defn- create-purchase-transaction
  "Given a trade map, creates the general currency
  transaction"
  [{:trade/keys [date
                 value
                 shares
                 fee-account
                 lot
                 fee
                 entity
                 account
                 commodity-account]
    :or {fee 0M}
    :as trade}]
  (let [currency-amount (+ value fee)
        items (cond-> [#:transaction-item{:action :credit
                                          :account account
                                          :quantity currency-amount
                                          :value currency-amount}
                       #:transaction-item{:action :debit
                                          :account commodity-account
                                          :quantity shares
                                          :value value}]
                (not= 0M fee) (conj #:transaction-item{:action :debit
                                                       :account fee-account
                                                       :quantity fee
                                                       :value fee}))]
    (assoc trade
           :trade/transaction
           #:transaction{:entity entity
                         :transaction-date date
                         :description (purchase-transaction-description trade)
                         :items items
                         :lot-items [#:lot-item{:lot lot
                                                :lot-action :buy
                                                :price (with-precision 4 (/ value shares))
                                                :shares shares}]})))

(defn- create-capital-gains-item
  [{:keys [quantity description long-term?]} trade]
  (let [[action effect] (if (< quantity 0)
                          [:debit "loss"]
                          [:credit "gains"])
        account-key (keyword
                      "trade"
                      (format "%s-capital-%s-account"
                              (if long-term? "lt" "st")
                              effect))
        account (account-key trade)]
    #:transaction-item{:action action
                       :account account
                       :quantity (.abs quantity)
                       :value (.abs quantity)
                       :memo description}))

(defn- create-capital-gains-items
  [{:trade/keys [gains] :as trade}]
  (mapv #(create-capital-gains-item % trade) gains))

(defn- create-sale-transaction-items
  [{:trade/keys [shares
                 value
                 account
                 commodity-account
                 fee
                 fee-account
                 gains]
    :or {fee 0M}
    :as trade}]
  (let [total-gains (->> gains
                         (map :quantity)
                         (reduce +))]
    (cond-> (conj (create-capital-gains-items trade)
                  #:transaction-item{:action :debit
                                     :account account
                                     :quantity (- value fee)
                                     :value (- value fee)}
                  #:transaction-item{:action :credit
                                     :account commodity-account
                                     :quantity shares
                                     :value (- value total-gains)})
      (not (zero? fee)) (conj #:transaction-item{:action :debit
                                                 :account fee-account
                                                 :quantity fee
                                                 :value fee}))))

(defn- create-sale-transaction
  "Given a trade map, creates the general currency
  transaction"
  [{:trade/keys [date account lot-items] :as trade}]
  (let [items (create-sale-transaction-items trade)
        transaction #:transaction{:entity (:account/entity account)
                                  :transaction-date date
                                  :description (sale-transaction-description trade)
                                  :items items
                                  :lot-items lot-items}]
    (if (v/has-error? transaction)
      (do
        (log/errorf "Unable to create the commodity sale transaction: %s" transaction)
        (throw (ex-info "Unable to create the commodity sale transaction." {:transaction transaction})))
      (assoc trade :trade/transaction transaction))))

(defn- create-lot
  "Given a trade map, creates and appends the commodity lot"
  [{:trade/keys [date
                 shares
                 commodity
                 account
                 price] :as trade}]
  (assoc trade :trade/lot {:id (util/temp-id)
                             :lot/account account
                             :lot/commodity commodity
                             :lot/purchase-date date
                             :lot/purchase-price (:price/price price)
                             :lot/shares-purchased shares
                             :lot/shares-owned shares}))

(defn- propagate-price-to-accounts
  "Propagate price change to affected accounts"
  [{:trade/keys [price commodity date commodity-account] :as trade}]
  ; For any account that references the commodity in this trade,
  ; that reflects a price-as-of date that is earlier than this
  ; trade date, update the value of the account based on the current
  ; price
  (assoc trade
         :trade/affected-accounts
         (->> (models/select (cond-> #:account{:commodity commodity
                                               :price-as-of [:<= date]
                                               :quantity [:> 0M]}

                               (util/live-id? (:id commodity-account))
                               (assoc :account/id [:!= (:id commodity-account)])))
              (map (fn [{:as act :account/keys [quantity]}]
                     (assoc act
                            :account/price-as-of date
                            :account/value (with-precision 2
                                             (* price quantity))))))))

(defn- put-purchase
  [{:trade/keys [transaction
                 lot
                 commodity
                 account
                 commodity-account
                 affected-accounts
                 price] :as trade}]
  ; First save the primary accounts so they all have ids
  ; Next save the transaction and lots, which will update the
  ; commodity account also
  ; Finall save the affected accounts
  (let [result (group-by db/model-type
                         (models/put-many
                           (concat [commodity-account
                                    lot
                                    commodity
                                    price
                                    account
                                    transaction]
                                   affected-accounts)))]
    (assoc trade
           :trade/transaction (first (:transaction result))
           :trade/fee-account (->> (:trade/account result)
                                   (filter #(= :expense (:account/type %)))
                                   first)
           :trade/account (->> (:account result)
                               (filter (system-tagged? :trading))
                               first)
           :trade/price (first (:price result))
           :trade/commodity-account (->> (:account result)
                                         (filter (system-tagged? :tradable))
                                         first))))

; expect
; either
;   :trade/commodity
;   :trade/account
; or
;   :trade/commodity-account
; :trade/date
; :trade/shares
; :trade/value
(defn buy
  [purchase]
  (with-ex-validation purchase ::models/purchase []
    (-> purchase
        append-commodity-account
        append-commodity
        append-accounts
        append-entity
        create-price
        update-accounts
        create-lot
        create-purchase-transaction
        propagate-price-to-accounts
        put-purchase)))

; (defn unbuy
;   "Reverses a commodity purchase"
;   [{transaction-id :id transaction-date :transaction-date}]
;   (with-transacted-storage (env :db)
;     (let [transaction (transactions/find transaction-id transaction-date)
;           lot (lots/find (-> transaction :lot-items first :lot-id))
;           commodity (commodities/find (:commodity-id lot))]
;       (when (not= (:shares-purchased lot) (:shares-owned lot))
;         (throw (IllegalStateException.
;                 "Cannot undo a purchase if shares have been sold from the lot")))
;       (transactions/delete transaction)
;       (lots/delete lot)
;       {:transaction transaction
;        :lot lot
;        :commodity commodity})))
 
(defn- acquire-lots
  "Given a trade map, finds the next lot containing
  shares that can be sold"
  [{:trade/keys [inventory-method commodity account] :as trade}]
  (assoc trade
         :trade/lots (models/select #:lot{:commodity commodity
                                          :account account
                                          :shares-owned [:!= 0M]}
                                    {:sort [[:lot/purchase-date
                                             (if (= :lifo inventory-method)
                                               :desc
                                               :asc)]]})))

(defn- process-lot-sale
  [{:as trade :trade/keys [price commodity]}
   {:as lot :lot/keys [shares-owned purchase-price]}
   shares-to-sell]
  (let [[shares-sold
         remaining-shares-to-sell
         new-lot-balance] (if (>= shares-owned shares-to-sell)
                            [shares-to-sell
                             0M
                             (- shares-owned shares-to-sell)]
                            [shares-owned
                             (- shares-to-sell shares-owned)
                             0M])
        sale-price (:price/price price)
        gain (.setScale
              (- (* shares-sold sale-price)
                 (* shares-sold purchase-price))
              2
              BigDecimal/ROUND_HALF_UP)
        cut-off-date (t/plus (:lot/purchase-date lot) (t/years 1))
        long-term? (>= 0 (compare cut-off-date
                                  (:trade/date trade)))]
    [(-> trade
         (update-in [:trade/lot-items]
                    #(conj % #:lot-item{:lot lot
                                        :lot-action :sell
                                        :shares shares-sold
                                        :price sale-price}))
         (update-in [:trade/updated-lots]
                    #(conj % (assoc lot :lot/shares-owned new-lot-balance)))
         (update-in [:trade/gains]
                    #(conj % {:description (format "Sell %s shares of %s at %s"
                                                   shares-sold
                                                   (:commodity/symbol commodity)
                                                   (format-decimal sale-price {:fraction-digits 3}))
                              :quantity gain
                              :long-term? long-term?})))
     remaining-shares-to-sell]))

(defn- process-lot-sales
  "Given a trade map, processes the lot changes and appends
  the new lot transactions and the affected lots"
  [{:trade/keys [lots shares] :as trade}]

  (when (> shares (->> lots
                       (map :lot/shares-owned)
                       (reduce + 0M)))
    (log/warnf "Attempt to sell more shares when owned: %s" trade))

  (loop [trd (assoc trade
                    :trade/lots []
                    :trade/gains []
                    :trade/lot-items [])
         shares-remaining (:trade/shares trd)
         lot (first lots)
         remaining-lots (rest lots)]
    (if lot
      (let [[adj-trd
             shares-to-be-sold] (process-lot-sale trd
                                                  lot
                                                  shares-remaining)]
        (if (zero? shares-to-be-sold)
          adj-trd
          (recur adj-trd shares-to-be-sold (first remaining-lots)  (rest remaining-lots))))
      (do
        (log/error "Unable to find a lot to sell shares " (prn-str trd))
        (throw (ex-info "Unable to find a lot to sell the shares" trd))))))
 
(defn- update-entity-settings
  [{:trade/keys [entity] :as trade}]
  (models/put (update-in entity
                         [:entity/settings]
                         #(merge % (-> trade
                                       (select-keys [:lt-capital-gains-account
                                                     :st-capital-gains-account
                                                     :lt-capital-loss-account
                                                     :st-capital-loss-account
                                                     :inventory-method])
                                       (util/qualify-keys :settings)))))
  trade)
 
(defn- find-or-create-account
  [account]
  (some #(% account)
        [models/find-by
         models/put]))
 
(defn- find-or-create-gains-account
  [{:trade/keys [entity]} term result]
  (find-or-create-account
    #:account{:entity entity
              :type (if (= "gains" result)
                      :income
                      :expense)
              :name (str (if (= "lt" term) "Long-term" "Short-term")
                         " Capital "
                         (if (= "gains" result) "Gains" "Losses"))}))
 
(defn- ensure-gains-account
  [{:trade/keys [entity] :as trade} [term result]]
  (let [k (keyword (str term "-capital-" result "-account"))]
    (if (k trade)
      trade
      (assoc trade k (or (k (:settings entity))
                           (find-or-create-gains-account trade
                                                         term
                                                         result))))))

(defn- ensure-gains-accounts
  "Ensures that the gain/loss accounts are present
  in the sale transaction."
  [trade]
  (->> (for [term ["lt" "st"]
             result ["gains" "loss"]]
         [term result])
       (reduce ensure-gains-account trade)))

(defn- put-sale
  [{:trade/keys [transaction
                 updated-lots
                 commodity
                 account
                 commodity-account
                 affected-accounts
                 price] :as trade}]
  ; First save the primary accounts so they all have ids
  ; Next save the transaction and lots, which will update the
  ; commodity account also
  ; Finall save the affected accounts
  (let [result (group-by db/model-type
                         (models/put-many
                           (concat updated-lots
                                   affected-accounts
                                   [commodity-account
                                    commodity
                                    price
                                    account
                                    transaction])))]
    (assoc trade
           :trade/transaction (first (:transaction result))
           :trade/fee-account (->> (:account result)
                                   (filter #(= :expense (:account/type %)))
                                   first)
           :trade/account (->> (:account result)
                               (filter (system-tagged? :trading))
                               first)
           :trade/price (first (:trade/price result))
           :trade/commodity-account (->> (:account result)
                                         (filter (system-tagged? :tradable))
                                         first)
           :trade/updated-lots (:lot result))))
(defn sell
  [sale]
  (with-ex-validation sale ::models/sale
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
         propagate-price-to-accounts
         put-sale)))

; (defn unsell
;   [{transaction-id :id transaction-date :transaction-date}]
;   (with-transacted-storage (env :db)
;     (let [transaction (transactions/find transaction-id transaction-date)]
;       (doseq [lot-item (:lot-items transaction)]
;         (let [lot (lots/find (:lot-id lot-item))]
;           (lots/update (update-in lot [:shares-owned] #(+ % (:shares lot-item))))))
;       (transactions/delete transaction))))
; 
; (defn- append-transfer-accounts
;   [{:keys  [from-account from-account-id to-account to-account-id commodity] :as context}]
;   (let [to-account (ensure-tag (or to-account
;                                    (accounts/find to-account-id))
;                                :trading)
;         from-account (ensure-tag (or from-account
;                                      (accounts/find from-account-id))
;                                  :trading)
;         [from-commodity-account
;          to-commodity-account] (map #(find-or-create-commodity-account
;                                       %
;                                       commodity)
;                                     [from-account to-account])]
;     (assoc context
;            :from-account from-account
;            :from-commodity-account from-commodity-account
;            :to-account to-account
;            :to-commodity-account to-commodity-account)))
; 
; (defn- process-transfer-lots
;   [{:keys [commodity from-account to-account shares] :as context}]
;   (let [to-move (->> (lots/search {:commodity-id (:id commodity)
;                                    :account-id (:id from-account)
;                                    :shares-owned [:> 0M]}
;                                   {:sort [:purchase-date]})
;                      (reduce (fn [acc {:keys [shares-owned] :as lot}]
;                                (if (>= (:shares acc) shares)
;                                  (reduced acc)
;                                  (-> acc
;                                      (update-in [:lots] conj lot)
;                                      (update-in [:shares] + shares-owned))))
;                              {:shares 0M
;                               :lots []})
;                      :lots)]
;     (if (empty? to-move)
;       (log/warnf "No lots found to transfer %s" (prn-str context))
;       (assoc context :lots (mapv #(lots/update
;                                    (assoc % :account-id (:id to-account)))
;                                  to-move)))))
; 
; (defn- create-transfer-transaction
;   [{:keys [commodity
;            from-commodity-account
;            to-commodity-account
;            transfer-date
;            shares]
;     :as context}]
;   (let [price (prices/most-recent commodity transfer-date)
;         value (* shares (:price price))
;         transaction (transactions/create {:entity-id (:entity-id commodity)
;                                           :transaction-date transfer-date
;                                           :description (format "Transfer %s shares of %s"
;                                                                shares
;                                                                (:symbol commodity))
;                                           :items [{:action :credit
;                                                    :quantity shares
;                                                    :value value
;                                                    :account-id (:id from-commodity-account)}
;                                                   {:action :debit
;                                                    :quantity shares
;                                                    :value value
;                                                    :account-id (:id to-commodity-account)}]})]
;     (assoc context :transaction transaction)))
; 
; (defn transfer
;   "Transfers a commodity from one account to another
; 
;   :commodity-id     - identifies the commodity to be moved
;   :shares           - the number of shares of the commodity to be transfered
;   :transaction-date - the date on which the transfer takes place
;   :from-account-id  - identifies the account from which the commodity is to be moved
;   :from-account     - the account from which the commodity is to be moved. Supplying this instead of :from-account-id bypasses the database lookup for the account.
;   :to-account-id    - identifies the account to which the commodity is to be moved
;   :to-account       - the account to which the commodity is to be moved. Supplying this instead of :to-account-id bypasses the database lookup for the account."
;   [transfer]
;   (let [validated (v/validate transfer ::transfer)]
;     (if (v/valid? validated)
;       (with-transacted-storage (env :db)
;         (some-> validated
;                 append-commodity
;                 append-transfer-accounts
;                 process-transfer-lots
;                 create-transfer-transaction
;                 (select-keys [:lots :transaction])))
;       validated)))
; 
; ; TODO: Simplify this by updating stowaway to allow operators in mass update
; (defn- apply-split-to-lot
;   [ratio lot]
;   (doseq [trx (lot-trans/search {:lot-id (:id lot)
;                                  :transaction-date (:purchase-date lot)})]
;     (lot-trans/update (-> trx
;                           (update-in [:price] #(with-precision 4 (/ % ratio)))
;                           (update-in [:shares] #(* % ratio)))))
;   (lots/update (-> lot
;                    (update-in [:shares-purchased] #(* % ratio))
;                    (update-in [:shares-owned] #(* % ratio))
;                    (update-in [:purchase-price] #(with-precision 4 (/ % ratio))))))
; 
; (defn- append-split-lots
;   [{:keys [commodity-id account-id] :as context}]
;   (assoc context :lots (lots/search {:commodity-id commodity-id
;                                      :account-id account-id
;                                      :shares-owned [:!= 0M]})))
; 
; (defn- append-split-ratio
;   [{:keys [shares-gained lots] :as context}]
;   (assert (seq lots) "No lots found to which to apply the split.")
;   (let [shares-owned (->> lots
;                           (map :shares-owned)
;                           (reduce + 0M))]
;     (assoc context :ratio (with-precision 4
;                             (/ (+ shares-owned shares-gained)
;                                shares-owned)))))
; 
; (defn- process-split-lots
;   [{:keys [lots ratio] :as context}]
;   (assoc context
;          :lots
;          (mapv #(apply-split-to-lot ratio %) lots)))
; 
; (defn- ratio->words
;   [ratio]
;   ; We'll need to expand this at some point to handle
;   ; reverse splits and stranger splits, like 3:2
;   (let [[n d] (cond->> [ratio 1]
;                 (< ratio 1)
;                 (map (comp int
;                            #(/ % ratio))))]
;     (format "%s for %s" n d)))
; 
; (defn- create-split-transaction
;   [{:keys [commodity
;            split-date
;            ratio
;            commodity-account
;            shares-gained] :as context}]
;   (assoc context
;          :transaction
;          (transactions/create {:entity-id (:entity-id commodity)
;                                :transaction-date split-date
;                                :description (format "Split shares of %s %s"
;                                                     (:symbol commodity)
;                                                     (ratio->words ratio))
;                                :items [{:action (if (< 0 shares-gained)
;                                                   :debit
;                                                   :credit)
;                                         :account-id (:id commodity-account)
;                                         :quantity (.abs shares-gained)
;                                         :value 0M}]})))
; 
; (defn- validate-split
;   [split]
;   (v/validate split ::split))
; 
; (defn split
;   "Records a stock split
; 
;   :commodity-id  - identifies the commodity being split
;   :split-date    - the date the split is effective
;   :shares-gained - the difference in the number of shares held before and after the split
;   :account-id    - the trading account through which the commodity was purchased"
; 
;   [split]
;   (let [validated (validate-split split)]
;     (if (v/valid? validated)
;       (with-transacted-storage (env :db)
;         (-> validated
;             append-commodity
;             append-accounts
;             append-split-lots
;             append-split-ratio
;             process-split-lots
;             create-split-transaction))
;       validated)))
