(ns clj-money.accounts
  (:require [clojure.string :as string]
            [clojure.set :as set]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clojure.core.async :as a]
            [dgknght.app-lib.models :as models]
            [dgknght.app-lib.web :refer [format-decimal]]
            [clj-money.decimal :as d]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.dates :as dates]
            [clj-money.util :as util :refer [id=]]))

(defprotocol ValuationData
  "Provides data need by the valuate function.

  The implementation is responsible for applying date boundaries to the
  data and making access to the data efficient"
  (fetch-entity
    [this account]
    "Fetches the entity for the given account")
  (fetch-balance
    [this account]
    "Fetches the raw balance (not price adjusted) for an account")
  (fetch-lots
    [this account]
    "Fetches the lots for a given trading account")
  (fetch-lot-items
    [this lot]
    "Fetches the lot items for a given lot")
  (fetch-price
    [this commodity]
    "Fetches the current price for the given commodity"))

(def account-types
  "The list of valid account types in standard presentation order"
  [:asset :liability :equity :income :expense])

(defn type?
  ([type]
   #(type? % type))
  ([account type]
   (= type (:account/type account))))

(defn action?
  [value]
  (-> value #{:credit :debit} boolean))

(defn expense?
  [{:account/keys [type]}]
  (= :expense type))

(defn- aggr
  [attr {:keys [oper initial allow-nil?] :or {oper d/+ initial 0M allow-nil? false}} coll]
  (let [f (if allow-nil? identity (constantly true))]
    (->> coll
       (map attr)
       (filter f)
       (reduce oper initial))))

(defn- sum
  ([attr coll]
   (sum attr {} coll))
  ([attr opts coll]
  {:pre [coll]}
   (aggr attr opts coll)))

(defn- eval-children
  [{:account/keys [children] :as account}]
  (let [children-value (->> children
                            (mapcat (juxt :account/value :account/children-value))
                            (filter identity)
                            (reduce d/+ 0M))
        children-cost-basis (sum :account/cost-basis {:allow-nil? true} children)
        value (:account/value account 0M)
        cost-basis (d/+ children-cost-basis value)
        total-value (d/+ children-value value)]
    (merge account
           (cond-> {:account/children-value children-value
                    :account/total-value total-value
                    :account/has-children? true}

             (not (d/zero? children-cost-basis))
             (assoc :account/cost-basis cost-basis
                    :account/gain (d/- total-value cost-basis))))))

(defn nest
  ([accounts] (nest {} accounts))
  ([{:keys [types]
     :or {types account-types}}
    accounts]
   (let [by-type (->> accounts
                      (map #(assoc % :account/total-value (:account/value %)))
                      (group-by :account/type))]
     (mapv #(hash-map :type %
                      :accounts (models/nest
                                  {:decorate-parent-fn eval-children
                                   :sort-key-fn :account/name
                                   :children-key :account/children
                                   :parent-fn (comp :id
                                                    :account/parent)}
                                  (get-in by-type [%])))
           types))))

(defn unnest
  [types]
  (->> types
       (mapcat :accounts)
       (models/unnest {:children-key :account/children
                       :path-key :account/path
                       :path-segment-fn :account/name
                       :parent-ids-key :account/parent-ids})))

(defn with-descendants
  "Given an account and a sequence of accounts that have been unnested (and
  so have :account/parent-ids), returns the account followed by those of
  its descendants that are in the sequence."
  [{:keys [id] :as account} accounts]
  (cons account
        (filter #(some #{id} (:account/parent-ids %))
                accounts)))

(defn left-side?
  "Returns truthy if the specified account is asset or expense, falsey if anything else"
  [{:account/keys [type] :as account}]
  {:pre [account (:account/type account)]}

  (#{:asset :expense} type))

(defn polarizer
  [action account]
  (d/* (if (left-side? account) (d/d 1) (d/d -1))
       (if (= :debit action) (d/d 1) (d/d -1))))

(defn polarize-quantity
  "Given a quantity, action, and an account, returns the quantity
  vis a vis the account (i.e., positive or negative)."
  ([input]
   (polarize-quantity ((some-fn :quantity
                                :transaction-item/quantity)
                       input)
                      ((some-fn :action
                                :transaction-item/action)
                       input)
                      ((some-fn :account
                                :transaction-item/account)
                       input)))
  ([quantity action account]
   {:pre [quantity
          action
          (#{:debit :credit} action)
          account
          (:account/type account)]}
   (d/* quantity
        (polarizer action account))))

(defn derive-action
  "Given a quantity (either positve or negative) and an
  account, returns the appropriate action (debit or credit)"
  [quantity account]
  (if (< quantity 0)
    (if (left-side? account)
      :credit
      :debit)
    (if (left-side? account)
      :debit
      :credit)))

(defn ->transaction-item
  "Given a quantity and an account, returns a unilateral transaction item
  with appropriate attributes.

  This is used in the UI, where various parts of the information may
  not yet have been entered by the user."
  [{:keys [quantity account]}]
  (cond-> {:transaction-item/action :credit}
    quantity      (assoc :transaction-item/quantity (d/abs quantity))
    account       (assoc :transaction-item/account (util/->entity-ref account))
    (and quantity
         account) (assoc :transaction-item/action
                         (derive-action quantity account))))

(defn ->>criteria
  ([accounts] (->>criteria {} accounts))
  ([{:keys [account-attribute
            date-attribute
            earliest-date
            latest-date
            entity-type]
     :or {account-attribute :transaction-item/account
          date-attribute :transaction/transaction-date
          entity-type :transaction-item}}
    accounts]
   (let [range (->> accounts
                    (map :account/transaction-date-range)
                    (filter identity)
                    (reduce (fn [m r]
                              (apply dates/push-boundary m r))
                            []))]
     (util/entity-type
       {account-attribute (if (= 1 (count accounts))
                            (util/->entity-ref (first accounts))
                            {:id [:in (->> accounts
                                           (map :id)
                                           set)]})
        date-attribute [:between
                        (or (first range) earliest-date)
                        (or (last range) latest-date)]}
       entity-type))))

(defn ->criteria
  [account & [opts]]
  (->>criteria opts [account]))

(defn- match-path?
  [path terms]
  (loop [t (first terms) tt (rest terms) p (first path) pp (rest path)]
    (when (and t p)
      (if (string/includes? p t)
        (if (seq tt)
          (recur (first tt) (rest tt) (first pp) (rest pp))
          true)
        (recur t tt (first pp) (rest pp))))))

(defn find-by-path
  ([accounts]
   #(find-by-path % accounts))
  ([term accounts]
   (filter #(and (not (:account/hidden %))
                 (match-path? (map string/lower-case (:account/path %))
                              (map string/lower-case (string/split term #"/|:"))))
           accounts)))

(defn- tagged?
  [account k tag]
  (let [tags (if (set? tag) tag #{tag})]
    (seq (set/intersection (k account) tags))))

(defn user-tagged?
  "Given an account and a tag or a set of tags, returns true
  if the account :user-tags contains any of the given tags.
  Given a tag or set of tags, returns a function that takes an
  account as an argument and returns true of the account :user-tags
  contain any of the given tags."
  ([tag]
   #(user-tagged? % tag))
  ([account tag]
   (tagged? account :account/user-tags tag)))

(defn system-tagged?
  "Given an account and a tag or a set of tags, returns true
  if the account :system-tags contains any of the given tags.
  Given a tag or set of tags, returns a function that takes an
  account as an argument and returns true of the account :system-tags
  contain any of the given tags."
  ([tag]
   #(system-tagged? % tag))
  ([account tag]
   (tagged? account :account/system-tags tag)))

(defn trading?
  [account]
  (system-tagged? account :trading))

(defn tradable?
  [account]
  (system-tagged? account :tradable))

(defn transfer-destination?
  "Returns true if shares of a commodity can be transferred into the account:
  a trading account, or an asset account that can become one. A commodity
  account (tagged :tradable) can't receive a transfer, because it holds one
  commodity beneath a trading account."
  [account]
  (and (= :asset (:account/type account))
       (not (tradable? account))))

(defn parent-only?
  [account]
  (system-tagged? account :parent-only))

(defn format-quantity
  [quantity {:account/keys [commodity]}]
  (format-decimal quantity
                  (if (= "USD" (:commodity/symbol commodity))
                    {:fraction-digits 2}
                    {:fraction-digits 4})))

(defn- estimate-gain
  "Estimates the gain (or loss, if negative) realized by selling the given
  value of shares from the given lots at the given price, taking the lots
  in purchase-date order, oldest first for :fifo or newest first for :lifo."
  [sale-value price lots inventory-method]
  (let [sorted (cond-> (sort-by :lot/purchase-date t/before? lots)
                 (= :lifo inventory-method) reverse)]
    (loop [to-sell (d// sale-value price)
           [{:lot/keys [shares-owned purchase-price]} & remaining] sorted
           gain 0M]
      (if (and shares-owned (d/< 0M to-sell))
        (let [shares (if (d/< to-sell shares-owned) to-sell shares-owned)]
          (recur (d/- to-sell shares)
                 remaining
                 (d/+ gain (d/* shares (d/- price purchase-price)))))
        (d/round gain 2)))))

(defn- ->allocation-rec
  [{:keys [target-percentage account] :as m} working-total]
  (let [percentage (d// target-percentage 100M)
        target-value (d/* working-total
                          percentage)
        current-value (:account/value account)
        current-percentage (d/round (d// current-value
                                         working-total)
                                    4)
        raw-adj-value (d/- target-value
                           current-value)
        adj-value (if (< (d/abs raw-adj-value) 100M)
                    0M
                    (d/* 100M
                         (d/round (d// raw-adj-value
                                       100M))))]
    (assoc m
           :current-value current-value
           :current-percentage current-percentage
           :target-value target-value
           :adj-value adj-value)))

(defn- +est-gain
  "Adds the estimated gain or loss of the sale to an allocation record that
  sells shares"
  [lots-by-commodity inventory-method]
  (fn [{:keys [account adj-value current-value] :as rec}]
    (let [lots (seq (get lots-by-commodity
                         (get-in account [:account/commodity :id])))
          quantity (:account/quantity account 0M)]
      (if (and lots
               (d/< adj-value 0M)
               (d/< 0M quantity))
        (assoc rec :est-gain (estimate-gain (d/abs adj-value)
                                            (d// current-value quantity)
                                            lots
                                            inventory-method))
        rec))))

(defn allocate
  "Returns the adjustments needed to bring the children of the given trading
  account to its target allocations.

  Options:
    :cash             - the cash to hold back from the allocation
    :withdrawal       - the cash to be withdrawn from the account
    :lots             - the account's lots, used to estimate the gain or loss
                        (:est-gain) of each sale
    :inventory-method - :fifo (default) or :lifo, the order in which lots
                        are sold"
  [{:account/keys [allocations total-value value]}
   find-account-fn
   & {:keys [cash withdrawal lots inventory-method]}]
  (let [cash-withheld (or cash value)
        withdrawal (or withdrawal (d/d 0))
        working-total (d/- total-value (d/+ cash-withheld withdrawal))
        lots-by-commodity (group-by (comp :id :lot/commodity)
                                    (filter #(d/< 0M (:lot/shares-owned % 0M))
                                            lots))
        result (mapv (comp #(->allocation-rec % working-total)
                           (fn [[id target-percentage]]
                             {:account (find-account-fn id)
                              :target-percentage target-percentage}))
                     allocations)
        net (->> result
                 (map :adj-value)
                 (reduce d/+ withdrawal))]
    (mapv (+est-gain lots-by-commodity inventory-method)
          (if (d/zero? net)
            result
            (update-in result [0 :adj-value] d/- net)))))

(defn- valuate-simple-accounts
  "Perform valuation of accounts that use the default entity commodity."
  [data accounts]
  {:pre [(seq accounts)]}
  (map #(assoc % :account/value (fetch-balance data %))
       accounts))

(def ^:private acquisition?
  (comp boolean #{:buy :receive} :lot-item/action))

(defn- append-lot-items
  [data commodity]
  (fn [lot]
    ; A lot is acquired by a purchase, or by receiving shares split off of
    ; another lot in a transfer. Every other item reduces the shares owned.
    ; Without an acquisition on or before the as-of date, the lot isn't
    ; held yet and is left out of the valuation.
    (let [items (fetch-lot-items data lot)
          {[purchase] true sales false} (group-by acquisition? items)]
      (if purchase
        (let [shares-owned (d/- (:lot-item/shares purchase)
                                (sum :lot-item/shares (or sales [])))]
          (if (< 0M shares-owned)
            (let [price (fetch-price data commodity)
                  cost-basis (d/* (:lot-item/price purchase)
                                  shares-owned)
                  current-value (d/* price shares-owned)]
              (assoc lot
                     :lot/items items
                     :lot/shares-purchased (:lot-item/shares purchase)
                     :lot/cost-basis cost-basis
                     :lot/shares-owned shares-owned
                     :lot/current-price price
                     :lot/value current-value
                     :lot/gain (d/- current-value cost-basis)))
            lot))
        lot))))

(defn- valuate-commodity-account
  [data]
  (fn [{:as account :account/keys [commodity]} ]
    (if-let [lots (->> (fetch-lots data account)
                       (sort-by :lot/purchase-date t/after?)
                       (map (append-lot-items data commodity))
                       (filter :lot/items)
                       seq)]
      (assoc account
             :account/lots lots
             :account/shares-purchased (sum :lot/shares-purchased lots)
             :account/shares-owned (sum :lot/shares-owned lots)
             :account/cost-basis (sum :lot/cost-basis lots)
             :account/value (sum :lot/value lots)
             :account/gain (sum :lot/gain lots)
             :account/current-price (:lot/current-price (first lots)))
      ; Shares can arrive without a purchase (e.g., a transfer in), in which
      ; case there are no lots, and the balance is a quantity of shares
      ; that must be converted to a value using the current price
      (let [shares (fetch-balance data account)]
        (if (or (nil? shares) (d/zero? shares))
          (assoc account :account/value shares)
          (let [price (fetch-price data commodity)]
            (assoc account
                   :account/shares-owned shares
                   :account/current-price price
                   :account/value (d/* shares price))))))))

(defn- valuate-commodity-accounts
  [data accounts]
  (map (valuate-commodity-account data)
       accounts))

(defn- default-commodity?
  [{{:settings/keys [default-commodity]} :entity/settings}]
  (fn [{:account/keys [commodity]}]
    (id= commodity
         default-commodity)))

(defn valuate
  "Given a sequence of accounts, assess their value based on the given implementation
  of the ValuationData protocol."
  [data accounts]
  (let [{simple true commodity false} (group-by (default-commodity?
                                                  (fetch-entity data (first accounts)))
                                                accounts)]
    (->> (when (seq simple)
           (valuate-simple-accounts data simple))
         (concat (when (seq commodity)
                   (valuate-commodity-accounts data commodity)))
         (nest {:sort-key-fn :account/name})
         unnest)))

(defn multi-save
  [{:keys [process]} accounts]

  (let [input (a/chan)
        output (a/chan)]
    (a/pipeline-async 1
                      output
                      process
                      input)
    (let [res (a/reduce
                (fn [acc result]
                  (if-let [err (ex-message result)]
                    (update-in acc [:errors] conj err)
                    (-> acc
                        (update-in [:results] conj result)
                        (update-in [:succeeded] inc))))
                {:succeeded 0
                 :errors []
                 :results []}
                output)]
      (a/go (a/onto-chan! input accounts))
      res)))
