(ns clj-money.models.lots
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clj-time.coerce :refer [to-local-date
                                     to-sql-date]]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(s/def ::id integer?)
(s/def ::account-id integer?)
(s/def ::commodity-id integer?)
(s/def ::purchase-date validation/local-date?)
(s/def ::purchase-price decimal?)
(s/def ::shares-purchased decimal?)
(s/def ::shares-owned decimal?)
(s/def ::new-lot (s/keys :req-un [::account-id
                                  ::commodity-id
                                  ::purchase-date
                                  ::purchase-price
                                  ::shares-purchased]))
(s/def ::existing-lot (s/keys :req-un [::id
                                       ::account-id
                                       ::commodity-id
                                       ::purchase-date
                                       ::shares-purchased
                                       ::shares-owned]))

(defn- after-read
  [lot]
  (-> lot
      (storage/tag ::models/lot)
      (update-in [:purchase-date] to-local-date)))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read
          (storage/select s
                          (storage/tag criteria ::models/lot)
                          options)))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (merge options {:limit 1})))))

(defn find-by-id
  [storage-spec id]
  (find-by storage-spec {:id id}))

(defn- before-save
  [lot]
  (-> lot
      (storage/tag ::models/lot)
      (update-in [:purchase-date] to-sql-date)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn- before-validation
  [lot storage]
  (assoc lot :account (accounts/find-by-id storage (:account-id lot))))

(defn- account-is-an-asset?
  [_ lot]
  (= :asset (-> lot :account :type)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial account-is-an-asset? storage)
                           [:account-id]
                           "The account must be an asset account")])

(defn create
  [storage lot]
  (with-storage [s storage]
    (let [lot (before-validation lot s)]
      (with-validation lot ::new-lot (validation-rules s)
        (as-> lot l
          (before-save l)
          (storage/create s l)
          (after-read l))))))

(defn select-by-commodity-id
  [storage-spec commodity-id]
  (if commodity-id
    (search storage-spec {:commodity-id commodity-id})
    []))

(defn update
  [storage lot]
  (with-storage [s storage]
    (let [lot (before-validation lot s)]
      (with-validation lot ::existing-lot (validation-rules s)
        (storage/update s (before-save lot))
        (find-by-id s (:id lot))))))

(defn- lot-unrealized-gains
  [{:keys [purchase-price
           commodity-id
           shares-owned]}
   price-map]
  (let [cost (* purchase-price shares-owned)
        price (or (price-map commodity-id)
                  0M)
        value (* price shares-owned)]
    (when (= 0M price)
      (log/errorf "Unable to find price for commodity %s to calculate unrealized gains" commodity-id))
    (- value cost)))

(defn unrealized-gains
  [storage-spec entity-id as-of]
  (with-storage [s storage-spec]
    (let [lots (search s {[:commodity :entity-id] entity-id
                          :purchase-date [:<= as-of]})
          commodity-prices (if (seq lots)
                             (->> (commodities/search s {:id (->> lots
                                                                  (map :commodity-id)
                                                                  set)})
                                  (map (juxt :id
                                             #(:price (prices/most-recent s % as-of))))
                                  (into {}))
                             {})]
      (->> lots
           (map #(lot-unrealized-gains % commodity-prices))
           (reduce + 0M)))))

(defn delete
  [storage-spec lot]
  (with-storage [s storage-spec]
    (storage/delete s lot)))
