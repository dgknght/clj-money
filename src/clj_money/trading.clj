(ns clj-money.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.util :refer [format-number
                                    pprint-and-return]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-transacted-storage]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.lots :as lots]
            [clj-money.models.lot-transactions :as lot-transactions]))

(defn- create-price
  "Given a context, calculates and appends the share price"
  [{:keys [storage shares value commodity-id trade-date] :as context}]
  (assoc context :price (prices/create storage
                                       {:commodity-id commodity-id
                                        :trade-date trade-date
                                        :price (/ value shares)})))

(defn- acquire-commodity
  "Given a purchase context, appends the commodity"
  [{:keys [commodity-id storage] :as context}]
  (assoc context :commodity (commodities/find-by-id storage commodity-id)))

(defn- find-commodity-account
  [storage parent-id symbol]
  (first (accounts/search
           storage
           {:parent-id parent-id
            :name symbol})))

(defn- create-commodity-account
  [storage parent-id symbol]
  (accounts/create storage {:name symbol
                            :type :asset
                            :content-type :commodity
                            :parent-id parent-id}))

(defn- acquire-accounts
  "Give a purchase context, acquires the accounts
  necessary to complete the purchase"
  [{:keys [account-id storage]
    {symbol :symbol} :commodity
    :as context}]
  (-> context
      (assoc :account (accounts/find-by-id storage account-id))
      (assoc :commodity-account (some #(% storage account-id symbol)
                                      [find-commodity-account
                                       create-commodity-account]))))

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
  [{:keys [storage trade-date value] :as context}]
  (assoc context
         :transaction
         (transactions/create
           storage
           {:entity-id (-> context :account :entity-id)
            :transaction-date trade-date
            :description (purchase-transaction-description context)
            :items [{:action :credit
                     :account-id (:account-id context)
                     :amount value}
                    {:action :debit
                     :account-id (-> context :commodity-account :id)
                     :amount value}]})))

(defn- create-sale-transaction
  "Given a purchase context, creates the general currency
  transaction"
  [{:keys [storage trade-date value] :as context}]
  (assoc context
         :transaction
         (transactions/create
           storage
           {:entity-id (-> context :account :entity-id)
            :transaction-date trade-date
            :description (purchase-transaction-description context)
            :items [{:action :debit
                     :account-id (:account-id context)
                     :amount value}
                    {:action :credit
                     :account-id (-> context :commodity-account :id)
                     :amount value}]})))

(defn- create-lot
  "Given a purchase context, creates and appends the commodity lot"
  [{:keys [storage trade-date shares commodity-id account-id] :as context}]
  (assoc context :lot (lots/create storage {:account-id account-id
                                            :commodity-id commodity-id
                                            :purchase-date trade-date
                                            :shares-purchased shares})))

(defn- create-lot-transaction
  "Given a context, creates and appends the lot transaction"
  [{storage :storage :as context}]
  (assoc context
         :lot-transaction
         (lot-transactions/create
           storage
           (-> context
               (select-keys [:account-id
                             :commodity-id
                             :trade-date
                             :shares])
               (assoc :action :buy)
               (assoc :price (-> context :price :price))))))

; expect
; :commodity-id
; :account-id
; :trade-date
; :shares
; :value
(defn buy
  [storage-spec purchase]
  (with-transacted-storage [s storage-spec]
    (->> (assoc purchase :storage s)
         acquire-commodity
         acquire-accounts
         create-price
         create-purchase-transaction
         create-lot
         create-lot-transaction)
    ; validate the input
    ))

(defn- find-lot
  "Given a sell context, finds the next lot containing
  shares that can be sold"
  [{:keys [storage] :as context}]
  (->> (select-keys context [:commodity-id :account-id])
       (lots/search storage)
       (filter #(> (:shares-owned %) 0)) ; should really do this in the database query
       (sort-by :created-at) ; this is FIFO, need to handle FILO
       first))

(defn- process-lot-sales
  "Given a sell context, processes the lot changes and appends
  the new lot transactions and the affected lots"
  [context]
  (loop [context context
         shares-remaining (:shares context)]
    (if-let [lot (find-lot context)]
      (let [shares-owned (:shares-owned lot)
            lot-balance (- shares-owned shares-remaining)
            adj-lot (assoc lot :shares-owned (if (< lot-balance 0)
                                               0
                                               lot-balance))]

        (throw (ex-info "not implemented" {}))

        (recur context (- shares-remaining (:shares-owned adj-lot))))
      (throw (ex-info "Unable to find a lot to sell the shares"
                      {:context context})))))

(defn sell
  [storage-spec sale]
  (with-transacted-storage [s storage-spec]
    (->> (assoc sale :storage s)
         acquire-commodity
         acquire-accounts
         create-price
         create-sale-transaction
         process-lot-sales)))
