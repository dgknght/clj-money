(ns clj-money.accounts
  (:refer-clojure :exclude [+ - * / zero? abs format])
  (:require [clojure.string :as string]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [dgknght.app-lib.models :as models]
            [dgknght.app-lib.web :refer [format-decimal]]
            [clj-money.decimal :as d]
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.util :as util :refer [model= format]]))

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

(defn left-side?
  "Returns truthy if the specified account is asset or expense, falsey if anything else"
  [{:account/keys [type] :as account}]
  {:pre [account (:account/type account)]}

  (#{:asset :expense} type))

(defn- polarizer
  [action account]
  (d/* (if (left-side? account) (d/d 1) (d/d -1))
       (if (= :debit action) (d/d 1) (d/d -1))))

(defn polarize-quantity
  "Given a transaction item and an account, returns the quantity of the
  transaction item vis a vis the account (i.e., positive or negative)."
  [quantity action account]
  {:pre [quantity
         action
         (#{:debit :credit} action)
         account
         (:account/type account)]}
  (d/* quantity
       (polarizer action account)))

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
  "Given a quantity and an account, returns a transaction item
  with appropriate attributes"
  [{:keys [quantity account]}]
  (cond-> {:transaction-item/action :credit}
    quantity      (assoc :transaction-item/quantity (d/abs quantity))
    account       (assoc :transaction-item/account (util/->model-ref account))
    (and quantity
         account) (assoc :transaction-item/action
                                  (derive-action quantity account))))

(defn ->>criteria
  ([accounts] (->>criteria {} accounts))
  ([{:keys [account-attribute
            date-attribute
            earliest-date
            latest-date
            model-type]
     :or {account-attribute :transaction-item/account
          date-attribute :transaction/transaction-date
          model-type :transaction-item}}
    accounts]
   ^{:clj-money.db/type model-type}
   {account-attribute (if (= 1 (count accounts))
                        (util/->model-ref (first accounts))
                        {:id [:in (->> accounts
                                       (map :id)
                                       set)]})
    date-attribute [:between
                    (or (->> accounts
                             (map :account/earliest-transaction-date)
                             (filter identity)
                             (sort t/before?)
                             first)
                        earliest-date)
                    (or (->> accounts
                             (map :account/latest-transaction-date)
                             (filter identity)
                             (sort t/after?)
                             first)
                        latest-date)]}))

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
   (filter #(match-path? (map string/lower-case (:account/path %))
                         (map string/lower-case (string/split term #"/|:")))
           accounts)))

(defn user-tagged?
  ([tag]
   #(user-tagged? % tag))
  ([{:account/keys [user-tags]} tag]
   (contains? user-tags tag)))

(defn system-tagged?
  ([tag]
   #(system-tagged? % tag))
  ([{:account/keys [system-tags]} tag]
   (contains? system-tags tag)))

(defn format-quantity
  [quantity {:keys [commodity]}]
  (format-decimal quantity
                  (if (= "USD" (:symbol commodity))
                    {:fraction-digits 2}
                    {:fraction-digits 4})))

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

(defn allocate
  [{:account/keys [allocations total-value value]} find-account-fn & {:keys [cash withdrawal]}]
  (let [cash-withheld (or cash value)
        withdrawal (or withdrawal (d/d 0))
        working-total (d/- total-value (d/+ cash-withheld withdrawal))
        result (mapv (comp #(->allocation-rec % working-total)
                           (fn [[id target-percentage]]
                             {:account (find-account-fn id)
                              :target-percentage target-percentage}))
                     allocations)
        net (->> result
                 (map :adj-value)
                 (reduce d/+ withdrawal))]
    (if (d/zero? net)
      result
      (update-in result [0 :adj-value] d/- net))))

(defn- valuate-simple-accounts
  "Perform valuation of accounts that use the default entity commodity."
  [data accounts]
  {:pre [(seq accounts)]}
  (map #(assoc % :account/value (fetch-balance data %))
         accounts))

(defn- valuate-commodity-account
  [{:as account :account/keys [commodity]} data]
  (let [price (fetch-price data commodity)
        lots (->> (fetch-lots data account)
                  (sort-by :lot/purchase-date t/after?)
                  (mapv (fn [lot]
                          (let [[purchase & sales :as items] (fetch-lot-items data lot)
                                _ (assert (seq items) (format "No lot items found for %s" lot))
                                shares-owned (d/- (:lot-item/shares purchase)
                                                  (sum :lot-item/shares sales))
                                cost-basis (d/* (:lot-item/price purchase)
                                                shares-owned)
                                current-value (d/* price shares-owned)]
                            (assoc lot
                                   :lot/items items
                                   :lot/cost-basis cost-basis
                                   :lot/shares-owned shares-owned
                                   :lot/current-price price
                                   :lot/value current-value
                                   :lot/gain (d/- current-value cost-basis))))))]
    (if (seq lots)
      (assoc account
             :account/lots lots
             :account/shares-owned (sum :lot/shares-owned lots)
             :account/cost-basis (sum :lot/cost-basis lots)
             :account/value (sum :lot/value lots)
             :account/gain (sum :lot/gain lots)
             :account/current-price price)
      account)))

(defn- valuate-commodity-accounts
  [opts accounts]
  (map #(valuate-commodity-account % opts)
       accounts))

(defn valuate
  "Given a sequence of accounts, assess their value based on the given implementation
  of the ValuationData protocol."
  [data accounts]
  (let [default-commodity? (fn [{:account/keys [commodity] :as a}]
                             (model= commodity
                                     (get-in (fetch-entity data a)
                                             [:entity/settings
                                              :settings/default-commodity])))
        {simple true commodity false} (group-by default-commodity?
                                                accounts)]
    (->> (when (seq simple)
           (valuate-simple-accounts data simple))
         (concat (when (seq commodity)
                   (valuate-commodity-accounts data commodity)))
         (nest {:sort-key-fn :account/name})
         unnest)))
