(ns clj-money.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.validation :as v :refer [with-ex-validation]]
            [clj-money.decimal :as d]
            [clj-money.accounts :refer [system-tagged?]]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]
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

(defn- find-price
  [attr]
  (or (models/find-by attr)
      attr))

(defn- create-price
  "Given a trade map, calculates and appends the share price"
  [{:trade/keys [shares value commodity date] :as trade}]
  (-> trade
      (update-in [:trade/commodity-account :account/price-as-of]
                 #(dates/latest % date))
      (assoc :trade/price (assoc (find-price #:price{:commodity commodity
                                                     :trade-date date})
                                 :price/value (with-precision 4 (/ value shares))))))

(defn- push-commodity-price-boundary
  [{:trade/keys [date] :as trade}]
  (update-in trade
             [:trade/commodity]
             dates/push-model-boundary
             :commodity/price-date-range
             date))

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
      (assoc-in [:trade/commodity-account :account/commodity-price] (:price/value price))))

(defn- append-entity
  [{{:account/keys [entity]} :trade/account :as trade}]
  (update-in trade [:trade/entity] (fnil (models/resolve-ref :entity)
                                         entity)))

(defn- sale-transaction-description
  [{:trade/keys [shares]
    {:commodity/keys [symbol]} :trade/commodity
    {:price/keys [value]} :trade/price}]
  (format "Sell %,1.3f shares of %s at %,1.3f"
          shares
          symbol
          value))

(defn- purchase-transaction-description
  [{:trade/keys [shares dividend? value price]
    {:commodity/keys [symbol]} :trade/commodity}]
  (if dividend?
    (format "Reinvest dividend of %,1.2f: purchase %,1.3f shares of %s at %,1.3f"
            value
            shares
            symbol
            (:price/value price))
    (format "Purchase %,1.3f shares of %s at %,1.3f"
            shares
            symbol
            (:price/value price))))

(defn- dividend-transaction-description
  [{{:commodity/keys [symbol]} :trade/commodity}]
  (format "Dividend received from %s" symbol))

; we'll either get a fn that gets a meaningful basis, or we just supply
; random values on the assumption propagation will happen separately
(defn- random-item-basis [& _]
  {:transaction-item/balance 0M
   :transaction-item/index (rand-int Integer/MAX_VALUE)})

(defn- create-dividend-transaction
  "When :dividend? is true, creates the transaction for
  the receipt of the dividend"
  [{:trade/keys [dividend?
                 dividend-account
                 account
                 value
                 entity
                 date]
    :as trade}
   {:keys [item-basis]}]
  (if dividend?
    (if dividend-account
      (let [dividend-basis (item-basis dividend-account)
            account-basis (item-basis account)]
        (assoc trade
               :trade/dividend-transaction
               #:transaction{:entity entity
                             :transaction-date date
                             :description (dividend-transaction-description trade)
                             :items [#:transaction-item{:action :credit
                                                        :account dividend-account
                                                        :quantity value
                                                        :index (inc (:transaction-item/index dividend-basis))
                                                        :balance (+ (:transaction-item/balance dividend-basis)
                                                                    value)}
                                     #:transaction-item{:action :debit
                                                        :account account
                                                        :quantity value
                                                        :index (inc (:transaction-item/index account-basis))
                                                        :balance (+ (:transaction-item/balance account-basis)
                                                                    value)}]}))
      (throw (ex-info "Unable to apply the dividend because a dividend account was not specified"
                      trade)))
    trade))

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
    :as trade}
   {:keys [item-basis]}]
  (let [currency-amount (+ value fee)
        account-basis (item-basis account)
        commodity-basis (item-basis commodity-account)
        fee-basis (item-basis fee-account)
        items (cond-> [#:transaction-item{:action :credit
                                          :account account
                                          :quantity currency-amount
                                          :value currency-amount
                                          :index (inc (:transaction-item/index account-basis))
                                          :balance (- (:transaction-item/balance account-basis)
                                                      currency-amount)}
                       #:transaction-item{:action :debit
                                          :account commodity-account
                                          :quantity shares
                                          :value value
                                          :index (inc (:transaction-item/index commodity-basis))
                                          :balance (+ (:transaction-item/balance commodity-basis)
                                                      shares)}]
                (not= 0M fee) (conj #:transaction-item{:action :debit
                                                       :account fee-account
                                                       :quantity fee
                                                       :value fee
                                                       :index (inc (:transaction-item/index fee-basis))
                                                       :balance (+ (:transaction-item/balance fee-basis)
                                                                   fee)}))]
    (assoc trade
           :trade/transaction
           #:transaction{:entity entity
                         :transaction-date date
                         :description (purchase-transaction-description trade)
                         :items items
                         :lot-items [#:lot-item{:lot lot
                                                :action :buy
                                                :price (with-precision 4 (/ value shares))
                                                :shares shares}]})))

(defn- create-capital-gains-item
  [trade {:keys [item-basis]}]
  (fn [{:keys [quantity description long-term?]}]
    (let [[action effect] (if (< quantity 0)
                            [:debit "loss"]
                            [:credit "gains"])
          account-key (keyword
                        "trade"
                        (format "%s-capital-%s-account"
                                (if long-term? "lt" "st")
                                effect))
          account (account-key trade)
          {:transaction-item/keys [balance index]} (item-basis account)]
      #:transaction-item{:action action
                         :account account
                         :quantity (d/abs quantity)
                         :value (d/abs quantity)
                         :memo description
                         :index (inc index)
                         :balance (+ quantity balance)})))

(defn- create-capital-gains-items
  [{:trade/keys [gains] :as trade} opts]
  (->> gains
       (map (create-capital-gains-item trade opts))
       (remove (comp zero?
                     :transaction-item/quantity))))

(defn- create-sale-transaction-items
  [{:trade/keys [shares
                 value
                 account
                 commodity-account
                 fee
                 fee-account
                 gains]
    :or {fee 0M}
    :as trade}
   {:keys [item-basis] :as opts}]
  (let [total-gains (->> gains
                         (map :quantity)
                         (reduce + 0M))
        subtotal (- value fee)
        account-basis (item-basis account)
        commodity-basis (item-basis commodity-account)
        fee-basis (when-not (zero? fee) (item-basis fee-account))]
    (cond-> (conj (create-capital-gains-items trade opts)
                  #:transaction-item{:action :debit
                                     :account account
                                     :quantity subtotal
                                     :value subtotal
                                     :index (inc (:transaction-item/index account-basis))
                                     :balance (+ (:transaction-item/balance account-basis)
                                                 subtotal)}
                  #:transaction-item{:action :credit
                                     :account commodity-account
                                     :quantity shares
                                     :value (- value total-gains)
                                     :index (inc (:transaction-item/index commodity-basis))
                                     :balance (- (:transaction-item/balance commodity-basis)
                                                 shares)})
      (not (zero? fee)) (conj #:transaction-item{:action :debit
                                                 :account fee-account
                                                 :quantity fee
                                                 :value fee
                                                 :index (inc (:transaction-item/index fee-basis))
                                                 :balance (+ (:transaction-item/balance fee-basis)
                                                             fee)}))))

(defn- create-sale-transaction
  "Given a trade map, creates the general currency
  transaction"
  [{:trade/keys [date account lot-items] :as trade} opts]
  (let [items (create-sale-transaction-items trade opts)]
    (update-in trade
               [:trade/transactions]
               (fnil conj [])
               #:transaction{:entity (:account/entity account)
                             :transaction-date date
                             :description (sale-transaction-description trade)
                             :items (vec items)
                             :lot-items lot-items})))

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
                           :lot/purchase-price (:price/value price)
                           :lot/shares-purchased shares
                           :lot/shares-owned shares}))

(defn- put-purchase
  [{:trade/keys [transaction
                 dividend-transaction
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
  (let [result (->> [[dividend-transaction]
                     [commodity-account
                      lot
                      commodity
                      price
                      account
                      transaction]]
                    (map #(filter identity %))
                    (filter seq)
                    (mapcat (partial models/put-many opts))
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
                                         first))))

(def ^:private default-opts
  {:item-basis random-item-basis})

; expect
; either
;   :trade/commodity
;   :trade/account
; or
;   :trade/commodity-account
; :trade/date
; :trade/shares
; :trade/value
;
; opts is passed to models/put-many
; and can also include a fn for getting the previous
; index and balance for transaction item roll-ups
(defn buy
  [purchase & {:as options}]
  (with-ex-validation purchase ::models/purchase []
    (let [opts (merge default-opts options)]
      (-> purchase
          append-commodity-account
          append-commodity
          append-accounts
          append-entity
          create-price
          push-commodity-price-boundary
          update-accounts
          create-lot
          (create-dividend-transaction opts)
          (create-purchase-transaction opts)
          (put-purchase opts)))))

(def buy-and-propagate
  (prop/+propagation buy))

(defn unbuy
  "Reverses a commodity purchase"
  [trx & {:as opts}]
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
    (models/delete-many opts [trx lot])
    {:transaction trx
     :lot lot
     :commodity commodity}))

(def unbuy-and-propagate
  (prop/+propagation unbuy))
 
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
        sale-price (:price/value price)
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
                                        :action :sell
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
  [{:trade/keys [lots shares commodity] :as trade}]

  (when (< (total-shares-owned lots) shares)
    (log/warnf "Attempt to sell more shares than owned: %s" trade))

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
      (let [msg (format "Unable to find a lot to sell %s shares of %s (%s) "
                        shares-remaining
                        (:commodity/name commodity)
                        (:commodity/symbol commodity))]
        (log/errorf "%s - %s" msg trd)
        (throw (ex-info msg trd))))))
 
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
  (some-fn models/find-by models/put))
 
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
                 (or (when account (models/find account :account))
                     (when-let [act (get-in entity [:entity/settings settings-key])]
                       (models/find act :account))
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
                 entity] :as trade}
   opts]
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
                    (models/put-many opts)
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
  [sale & {:as options}]
  (with-ex-validation sale ::models/sale
    (let [opts (merge default-opts options)]
      (-> sale
          append-commodity-account
          append-commodity
          append-accounts
          append-entity
          acquire-lots
          ensure-gains-accounts
          update-entity-settings
          create-price
          push-commodity-price-boundary
          update-accounts
          process-lot-sales
          (create-sale-transaction opts)
          (put-sale opts)))))

(def sell-and-propagate
  (prop/+propagation sell))

(defn unsell
  [trx & {:as opts}]
  (let [lot-items (models/select
                    (util/model-type {:transaction/_self trx}
                                     :lot-item))
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
    (models/put-many opts
                     (cons [::db/delete trx]
                           updated-lots))))

(def unsell-and-propagate
  (prop/+propagation unsell))

(defn- append-transfer-accounts
  [{:transfer/keys  [from-account
                     to-account
                     commodity]
    :as transfer}]
  (-> transfer
      (update-in [:transfer/from-account] (models/resolve-ref :account))
      (update-in [:transfer/to-account] (comp #(ensure-tag % :trading)
                                              (models/resolve-ref :account)))
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

(defn- append-most-recent-price
  [{:as transfer :transfer/keys [commodity date]}]
  (assoc transfer
         :transfer/most-recent-price
         (prices/most-recent commodity date)))

(defn- create-transfer-transaction
  [{:transfer/keys [commodity
                    most-recent-price
                    from-commodity-account
                    to-commodity-account
                    date
                    shares]
    :as transfer}
   {:keys [item-basis]}]
  (when-not most-recent-price
    (throw (ex-info "Unable to process transfer without most recent commodity price"
                    {:transfer transfer})))
  (let [value (d/* shares (:price/value most-recent-price))
        from-basis (item-basis from-commodity-account)
        to-basis (item-basis to-commodity-account)]
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
                                                    :account from-commodity-account
                                                    :index (inc (:transaction-item/index from-basis))
                                                    :balance (+ (:transaction-item/balance from-basis)
                                                                shares)}
                                 #:transaction-item{:action :debit
                                                    :quantity shares
                                                    :value value
                                                    :account to-commodity-account
                                                    :index (inc (:transaction-item/index to-basis))
                                                    :balance (+ (:transaction-item/balance to-basis)
                                                                shares)}]})))

(defn- put-transfer
  [{:transfer/keys [transaction
                    lots
                    to-commodity-account]
    :as transfer}
   opts]
  (let [result (->> (cond->> (cons transaction lots)
                      (util/temp-id? to-commodity-account)
                      (cons to-commodity-account))
                    (models/put-many opts)
                    (group-by util/model-type))]
    (merge transfer
           {:transfer/transaction (first (:transaction result))
            :transfer/lots (:lot result)})))

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

; opts is passed to models/put-many
; and can contain a function item-basis to use to
; calculate item roll-ups
(defn transfer
  "Transfers a commodity from one account to another

  :commodity    - the commodity to be moved
  :shares       - the number of shares of the commodity to be transfered
  :date         - the date on which the transfer takes place
  :from-account - the account from which the commodity is to be moved
  :to-account   - the account to which the commodity is to be moved"
  [transfer & {:as options}]
  (let [opts (merge default-opts options)]
    (with-ex-validation transfer ::models/transfer
      (some-> transfer
              (update-in [:transfer/commodity] (models/resolve-ref :commodity))
              append-transfer-accounts
              append-most-recent-price
              process-transfer-lots
              (create-transfer-transaction opts)
              (put-transfer opts)))))

(def transfer-and-propagate
  (prop/+propagation transfer))

(defn- append-split-lots
  [{:split/keys [commodity account] :as split}]
  (assoc split
         :split/lots
         (models/select #:lot{:commodity commodity
                              :account account
                              :shares-owned [:!= 0M]})))

(defn- append-split-ratio
  [{:split/keys [shares-gained lots] :as split}]
  (when (seq lots)
    (let [shares-owned (->> lots
                            (map :lot/shares-owned)
                            (reduce + 0M))]
      (assoc split :split/ratio (with-precision 4
                                  (/ (+ shares-owned shares-gained)
                                     shares-owned))))))

(defn- apply-ratio-to-lot-item
  [item ratio]
  (-> item
      (update-in [:lot-item/price] #(with-precision 4 (/ % ratio)))
      (update-in [:lot-item/shares] #(* % ratio))))

(defn- fetch-and-adjust-lot-items
  [lot ratio]
  (mapv #(apply-ratio-to-lot-item % ratio)
        (models/select #:lot-item{:lot lot})))

(defn- apply-ratio-to-lot
  [lot ratio]
  (-> lot
      (update-in [:lot/shares-purchased] #(* % ratio))
      (update-in [:lot/shares-owned] #(* % ratio))
      (update-in [:lot/purchase-price] #(with-precision 4 (/ % ratio)))))

(defn- adjust-lot-and-lot-items
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
  (let [{:keys [lots lot-items]} (adjust-lot-and-lot-items ratio lots)]
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
                           #(with-precision 1
                              (/ % ratio)))))]
    (format "%s for %s" n d)))

(defn- create-split-transaction
  [{:split/keys [commodity
                 date
                 ratio
                 commodity-account
                 shares-gained] :as split}
   {:keys [item-basis]}]
  (let [{:transaction-item/keys [index balance]} (item-basis commodity-account)]
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
                                                    :value 0M
                                                    :index (inc index)
                                                    :balance (+ balance shares-gained)}]})))

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
                    (models/put-many opts)
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

  [split & {:as options}]
  (let [opts (merge default-opts options)]
    (with-ex-validation split ::models/split
    (some-> split
            (update-in [:split/commodity] (models/resolve-ref :commodity))
            append-split-accounts
            append-split-lots
            append-split-ratio
            adjust-split-lots
            (create-split-transaction opts)
            (put-split opts)))))

(def split-and-propagate
  (prop/+propagation split))
