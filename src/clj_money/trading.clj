(ns clj-money.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.validation :as v :refer [with-ex-validation]]
            [clj-money.accounts :refer [system-tagged?]]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.prices :as prices]
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
(s/def :trade/dividend? boolean?)
(s/def :trade/dividend-account ::models/model-ref)

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
                :trade/fee-account
                :trade/dividend?
                :trade/dividend-account]))

(defmethod purchase-spec :separate [_]
  (s/keys :req [:trade/date
                :trade/shares
                :trade/value
                :trade/commodity
                :trade/account]
          :opt [:trade/fee
                :trade/fee-account
                :trade/dividend?
                :trade/dividend-account]))

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

(defn- create-price
  "Given a trade map, calculates and appends the share price"
  [{:trade/keys [shares value commodity date] :as trade}]
  (let [existing (models/find-by {:price/commodity commodity
                                  :price/trade-date date})
        price-value (with-precision 4 (/ value shares))
        price (if existing
                (assoc existing :price/price price-value)
                #:price{:commodity commodity
                        :trade-date date
                        :price price-value})]
    ; TODO: We should probably report this back to the user
    (when-not (= (:price/price price)
                 (:price/price existing))
      (log/debugf "Conflicting commodity price for %s on %s: %s vs %s"
                  (:commodity/symbol commodity)
                  date
                  (:price/price existing)
                  (:price/price price)))
    (-> trade
        (update-in [:trade/commodity-account :account/price-as-of]
                   #(dates/latest % date))
        (assoc :trade/price price))))

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
  (update-in trade [:trade/commodity] (models/resolve-ref :commodity)))

(defn- find-commodity-account
  [parent commodity]
  (when-let [result (models/find-by #:account{:parent parent
                                              :commodity commodity})]
    (ensure-tag result :tradable)))

(defn- create-commodity-account
  [parent {:as commodity :commodity/keys [entity]}]
  {:id (util/temp-id)
   :account/name (:commodity/symbol commodity)
   :account/type :asset
   :account/commodity commodity
   :account/parent parent
   :account/entity entity
   :account/system-tags #{:tradable}})

(defn- find-or-create-commodity-account
  [parent commodity]
  (some #(% parent commodity)
        [find-commodity-account
         create-commodity-account]))

(defn- append-accounts
  "Give a trade map, acquires the accounts
  necessary to complete the purchase"
  [{:trade/keys [account commodity]
    :as trade}]
  {:pre [(and account commodity)]}
  (-> trade
      (update-in [:trade/account]
                 (models/resolve-ref :account))
      (update-in [:trade/commodity-account]
                 #(if %
                    (models/resolve-ref % :account)
                    (find-or-create-commodity-account account commodity)))))

(defn- update-accounts
  [{:as trade :trade/keys [price]}]

  ; Note that :account/commodity-price is not saved, but is used in
  ; transactions in order to calculate the value of the account
  (-> trade
      (update-in [:trade/account] ensure-tag :trading)
      (update-in [:trade/commodity-account] ensure-tag :tradable)
      (assoc-in [:trade/commodity-account :account/commodity-price] (:price/price price))))

(defn- append-entity
  [{{:account/keys [entity]} :trade/account :as trade}]
  (update-in trade [:trade/entity] (fnil (models/resolve-ref :entity)
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

(defn- dividend-transaction-description
  [{:trade/keys [shares value]
    {:commodity/keys [symbol]} :trade/commodity
    {:price/keys [price]} :trade/price}]
  (format "Reinvest dividend of %s: purchase %s shares of %s at %s"
          value
          shares
          symbol
          (format-decimal price {:fraction-digits 3})))

(defn- create-dividend-transaction
  "When :dividend? is true, creates the transaction for
  the receipt of the dividend"
  [{:trade/keys [dividend? dividend-account account value entity date] :as trade}]
  (when dividend?
    (if dividend-account
      #:transaction{:entity entity
                    :transaction-date date
                    :description (dividend-transaction-description trade)
                    :items [#:transaction-item{:action :credit
                                               :account dividend-account
                                               :quantity value}
                            #:transaction-item{:action :debit
                                               :account account
                                               :quantity value}]}
      (throw (ex-info "Unable to apply the dividend because a dividend account was not specified"
                      trade)))))

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
    (update-in trade
               [:trade/transactions]
               (fnil conj [])
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
      (update-in trade [:trade/transactions] (fnil conj []) transaction))))

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

(defn- put-purchase
  [{:trade/keys [transactions
                 lot
                 commodity
                 account
                 commodity-account
                 price] :as trade}
   opts]
  ; First save the primary accounts so they all have ids
  ; Next save the transaction and lots, which will update the
  ; commodity account also
  ; Finally save the affected accounts
  (let [result (group-by util/model-type
                         (models/put-many
                           opts
                           (concat [commodity-account
                                    lot
                                    commodity
                                    price
                                    account]
                                   transactions)))]
    (assoc trade
           :trade/transactions (:transaction result) ; TODO: Prune out the propagated transactions
           :trade/fee-account (->> (:account result)
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
  [purchase & {:as opts}]
  (with-ex-validation purchase ::models/purchase []
    (let [prepped (-> purchase
                      append-commodity-account
                      append-commodity
                      append-accounts
                      append-entity
                      create-price
                      update-accounts
                      create-lot)
          div-trx (some-> prepped
                          create-dividend-transaction
                          models/put)
          res (-> prepped
                  create-purchase-transaction
                  (put-purchase opts))]
      (if div-trx
        (update-in res [:trade/transactions] #(cons div-trx %))
        res))))

(def buy-and-propagate
  (models/+propagation buy))

(defn unbuy
  "Reverses a commodity purchase"
  [trx]
  (let [lot (models/resolve-ref
              (get-in trx [:transaction/lot-items
                           0
                           :lot-item/lot])
              :lot)
        commodity (models/resolve-ref (:lot/commodity lot)
                                      :commodity)]
    (when (not= (:lot/shares-purchased lot) (:lot/shares-owned lot))
      (throw (IllegalStateException.
               "Cannot undo a purchase if shares have been sold from the lot")))
    (models/delete-many [trx lot])
    {:transaction trx
     :lot lot
     :commodity commodity}))
 
(defn- acquire-lots
  "Given a trade map, finds the next lot containing
  shares that can be sold"
  [{:trade/keys [inventory-method commodity account entity] :as trade}]
  (assoc trade
         :trade/lots (models/select #:lot{:commodity commodity
                                          :account account
                                          :shares-owned [:!= 0M]}
                                    {:sort [[:lot/purchase-date
                                             (if (= :lifo (or inventory-method
                                                              (:settings/inventory-method entity)))
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

(defn- total-shares-owned
  [lots]
  (->> lots
       (map :lot/shares-owned)
       (reduce + 0M)))

(defn- process-lot-sales
  "Given a trade map, processes the lot changes and appends
  the new lot transactions and the affected lots"
  [{:trade/keys [lots shares] :as trade}]

  (when (< (total-shares-owned lots) shares)
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
             unsold-shares] (process-lot-sale trd
                                                  lot
                                                  shares-remaining)]
        (if (zero? unsold-shares)
          adj-trd
          (recur adj-trd
                 unsold-shares
                 (first remaining-lots)
                 (rest remaining-lots))))
      (do
        (log/errorf "Unable to find a lot to sell shares %s" trd)
        (throw (ex-info "Unable to find a lot to sell the shares" trd))))))
 
(defn- update-entity-settings
  [trade]
  (let [settings (-> trade
                     (select-keys [:trade/lt-capital-gains-account
                                   :trade/st-capital-gains-account
                                   :trade/lt-capital-loss-account
                                   :trade/st-capital-loss-account
                                   :trade/inventory-method])
                     (update-keys #(keyword "settings" (name %)))
                     (update-vals #(if (map? %)
                                     (util/->model-ref %)
                                     %)))]
    (update-in trade
               [:trade/entity :entity/settings]
               #(merge settings %))))
 
(def ^:private find-or-create-account
  (some-fn models/find-by util/+temp-id))
 
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
  (let [naked-key (str term "-capital-" result "-account")
        trade-key (keyword "trade" naked-key)
        settings-key (keyword "settings" naked-key)]
    (update-in trade
               [trade-key]
               (fn [account]
                 (or account
                     (get-in entity [:entity/settings settings-key])
                     (find-or-create-gains-account trade term result))))))

(defn- ensure-gains-accounts
  "Ensures that the gain/loss accounts are present
  in the sale transaction."
  [trade]
  (->> (for [term ["lt" "st"]
             result ["gains" "loss"]]
         [term result])
       (reduce ensure-gains-account trade)))

(defn- put-sale
  [{:trade/keys [transactions
                 updated-lots
                 commodity
                 account
                 commodity-account
                 lt-capital-gains-account
                 lt-capital-loss-account
                 st-capital-gains-account
                 st-capital-loss-account
                 price
                 entity] :as trade}]
  ; First save the primary accounts so they all have ids
  ; Next save the transaction and lots, which will update the
  ; commodity account also
  ; Finally save the affected accounts
  (let [result (->> (concat [lt-capital-gains-account
                             lt-capital-loss-account
                             st-capital-gains-account
                             st-capital-loss-account
                             commodity-account
                             commodity
                             price
                             account
                             entity]
                            transactions
                            updated-lots)
                    (filter identity)
                    (models/put-many)
                    (group-by util/model-type))]
    (assoc trade
           :trade/transactions (:transaction result)
           :trade/fee-account (->> (:account result)
                                   (filter #(= :expense (:account/type %)))
                                   first)
           :trade/account (->> (:account result)
                               (filter (system-tagged? :trading))
                               first)
           :trade/price (first (:price result))
           :trade/commodity-account (->> (:account result)
                                         (filter (system-tagged? :tradable))
                                         first)
           :trade/updated-lots (:lot result))))
(defn sell
  [sale]
  (with-ex-validation sale ::models/sale
    (-> sale
        append-commodity-account
        append-commodity
        append-accounts
        append-entity
        acquire-lots
        ensure-gains-accounts
        update-entity-settings
        create-price
        update-accounts
        process-lot-sales
        create-sale-transaction
        put-sale)))

(defn unsell
  [trx]
  (let [lot-items (models/select
                    #:lot-item{:transaction trx
                               :transaction-date (:transaction/transaction-date trx)})
        lots (index-by :id
                       (models/select (util/model-type
                                        {:id [:in (map (comp :id :lot-item/lot)
                                                       lot-items)]}
                                        :lot)))
        updated-lots (vals (reduce (fn [lots lot-item]
                                     (update-in lots
                                                [(get-in lot-item [:lot-item/lot :id])
                                                 :lot/shares-owned]
                                                #(+ % (:lot-item/shares lot-item))))
                                   lots
                                   lot-items))]
    (models/put-many (cons [::db/delete trx]
                           updated-lots))))

(defn- append-transfer-accounts
  [{:transfer/keys  [from-account
                     to-account
                     commodity]
    :as transfer}]
  (-> transfer
      (update-in [:transfer/from-account] (models/resolve-ref :account))
      (update-in [:transfer/to-account] (models/resolve-ref :account))
      (assoc :transfer/from-commodity-account
             (models/find-by #:account{:commodity commodity
                                       :parent from-account}))
      (assoc :transfer/to-commodity-account
             (find-or-create-commodity-account to-account commodity))))

(defn- process-transfer-lots
  [{:transfer/keys [commodity
                    from-account
                    to-account
                    shares] :as context}]
  (let [lots (->> (models/select #:lot{:commodity commodity
                                       :account from-account
                                       :shares-owned [:> 0M]}
                                 {:sort [:lot/purchase-date]})
                  (reduce (fn [acc {:lot/keys [shares-owned] :as lot}]
                            (if (>= (:shares acc) shares)
                              (reduced acc)
                              (-> acc
                                  (update-in [:lots] conj lot)
                                  (update-in [:shares] + shares-owned))))
                          {:shares 0M
                           :lots []})
                  :lots)]
    (if (empty? lots)
      (log/warnf "No lots found to transfer %s" context)
      (assoc context
             :transfer/lots
             (mapv #(assoc % :lot/account to-account)
                   lots)))))

(defn- create-transfer-transaction
  [{:transfer/keys [commodity
                    from-commodity-account
                    to-commodity-account
                    date
                    shares]
    :as transfer}]
  (let [price (prices/most-recent commodity date)
        value (* shares (:price/price price))]
    (assoc transfer
           :transfer/transaction
           #:transaction{:entity (:commodity/entity commodity)
                         :transaction-date date
                         :description (format "Transfer %s shares of %s"
                                              shares
                                              (:commodity/symbol commodity))
                         :items [#:transaction-item{:action :credit
                                                    :quantity shares
                                                    :value value
                                                    :account from-commodity-account}
                                 #:transaction-item{:action :debit
                                                    :quantity shares
                                                    :value value
                                                    :account to-commodity-account}]})))

(defn- put-transfer
  [{:transfer/keys [transaction
                    lots
                    to-commodity-account]}]
  (let [result (->> (cond->> (cons transaction lots)
                      (util/temp-id? to-commodity-account)
                      (cons to-commodity-account))
                    models/put-many
                    (group-by util/model-type))]
    {:transfer/transaction (first (:transaction result))
     :transfer/lots (:lot result)}))

(s/def :transfer/date t/local-date?)
(s/def :transfer/from-account ::models/model-ref)
(s/def :transfer/to-account ::models/model-ref)
(s/def :transfer/commodity ::models/model-ref)
(s/def :transfer/shares decimal?)
(s/def ::models/transfer (s/keys :req [:transfer/date
                                       :transfer/shares
                                       :transfer/from-account
                                       :transfer/to-account
                                       :transfer/commodity]))

(defn transfer
  "Transfers a commodity from one account to another

  :commodity    - the commodity to be moved
  :shares       - the number of shares of the commodity to be transfered
  :date         - the date on which the transfer takes place
  :from-account - the account from which the commodity is to be moved
  :to-account   - the account to which the commodity is to be moved"
  [transfer]
  (with-ex-validation transfer ::models/transfer
    (some-> transfer
            (update-in [:transfer/commodity] (models/resolve-ref :commodity))
            append-transfer-accounts
            process-transfer-lots
            create-transfer-transaction
            put-transfer)))

(defn- append-split-lots
  [{:split/keys [commodity account] :as split}]
  (assoc split
         :split/lots
         (models/select #:lot{:commodity commodity
                              :account account
                              :shares-owned [:!= 0M]})))

(defn- append-split-ratio
  [{:split/keys [shares-gained lots] :as split}]
  (assert (seq lots) "No lots found to which to apply the split.")
  (let [shares-owned (->> lots
                          (map :lot/shares-owned)
                          (reduce + 0M))]
    (assoc split :split/ratio (with-precision 4
                                (/ (+ shares-owned shares-gained)
                                   shares-owned)))))

(defn- apply-ratio-to-lot-item
  [item ratio]
  (-> item
      (update-in [:lot-item/price] #(with-precision 4 (/ % ratio)))
      (update-in [:lot-item/shares] #(* % ratio))))

(defn- fetch-and-adjust-lot-items
  [lot ratio]
  (mapv #(apply-ratio-to-lot-item % ratio)
        (models/select #:lot-item{:lot lot
                                  :transaction-date (:lot/purchase-date lot)})))

(defn- apply-ratio-to-lot
  [lot ratio]
  (-> lot
      (update-in [:lot/shares-purchased] #(* % ratio))
      (update-in [:lot/shares-owned] #(* % ratio))
      (update-in [:lot/purchase-price] #(with-precision 4 (/ % ratio)))))

(defn- adjust-lot-and-fetch-lot-items
  [ratio lots]
  (reduce (fn [acc lot]
            (-> acc
                (update-in [:lots] conj (apply-ratio-to-lot lot ratio))
                (update-in [:lot-items] concat (fetch-and-adjust-lot-items lot ratio))))
          {:lots []
           :lot-items []}
          lots))

(defn- adjust-split-lots
  [{:split/keys [lots ratio] :as split}]
  (let [{:keys [lots lot-items]} (adjust-lot-and-fetch-lot-items ratio lots)]
    (assoc split
           :split/lots lots
           :split/lot-items lot-items)))

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
  [{:split/keys [commodity
                 date
                 ratio
                 commodity-account
                 shares-gained] :as split}]
  (assoc split
         :split/transaction
         #:transaction{:entity (:commodity/entity commodity)
                       :transaction-date date
                       :description (format "Split shares of %s %s"
                                            (:commodity/symbol commodity)
                                            (ratio->words ratio))
                       :items [#:transaction-item{:action (if (< 0 shares-gained)
                                                            :debit
                                                            :credit)
                                                  :account commodity-account
                                                  :quantity (.abs shares-gained)
                                                  :value 0M}]}))

(defn- append-split-accounts
  [{:as split :split/keys [commodity account]}]
  (-> split
      (update-in [:split/account] (models/resolve-ref :account))
      (assoc :split/commodity-account (models/find-by #:account{:commodity commodity
                                                                :parent account}))))

(defn- put-split
  [{:split/keys [transaction
                 lots
                 lot-items
                 ratio]}
   opts]
  (let [result (->> (cons transaction (concat lots lot-items))
                    (models/put-many (mapcat identity opts))
                    (group-by util/model-type))]
    {:split/transaction (first (:transaction result))
     :split/lots (:lot result)
     :split/lot-items (:lot-item result)
     :split/ratio ratio}))

(s/def :split/date t/local-date?)
(s/def :split/commodity ::models/model-ref)
(s/def :split/account ::models/model-ref)
(s/def :split/shares-gained decimal?)
(s/def ::models/split (s/keys :req [:split/date
                                    :split/commodity
                                    :split/account
                                    :split/shares-gained]))

(defn split
  "Records a stock split

  :commodity     - the commodity being split
  :date          - the date the split is effective
  :shares-gained - the difference in the number of shares held before and after the split
  :account       - the trading account through which the commodity was purchased"

  [split & {:as opts}]
  (with-ex-validation split ::models/split
    (-> split
        (update-in [:split/commodity] (models/resolve-ref :commodity))
        append-split-accounts
        append-split-lots
        append-split-ratio
        adjust-split-lots
        create-split-transaction
        (put-split opts))))

(def split-and-propagate
  (models/+propagation split))
