(ns clj-money.models.lots
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clj-time.coerce :refer [to-local-date
                                     to-sql-date]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.util :refer [->id]]
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
      (tag ::models/lot)
      (update-in [:purchase-date] to-local-date)))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/lot)
                          options)))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (merge options {:limit 1})))))

(defn find
  [lot-or-id]
  (find-by {:id (->id lot-or-id)}))

(defn- before-save
  [lot]
  (-> lot
      (tag ::models/lot)
      (update-in [:purchase-date] to-sql-date)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn- before-validation
  [lot]
  (assoc lot :account (accounts/find (:account-id lot))))

(defn- account-is-an-asset?
  [lot]
  (= :asset (-> lot :account :type)))

(def ^:private validation-rules
  [(validation/create-rule account-is-an-asset?
                           [:account-id]
                           "The account must be an asset account")])

(defn create
  [lot]
  (with-storage (env :db)
    (let [lot (before-validation lot)]
      (with-validation lot ::new-lot validation-rules
        (-> lot
            before-save
            storage/create
            after-read)))))

(defn select-by-commodity-id
  [commodity-id]
  (if commodity-id
    (search {:commodity-id commodity-id})
    []))

(defn update
  [lot]
  (with-storage (env :db)
    (let [lot (before-validation lot)]
      (with-validation lot ::existing-lot validation-rules
        (-> lot
            before-save
            storage/update)
        (find lot)))))

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
  [entity-id as-of]
  (with-storage (env :db)
    (let [lots (search {[:commodity :entity-id] entity-id
                        :purchase-date [:<= as-of]})
          commodity-prices (if (seq lots)
                             (->> (commodities/search {:id (->> lots
                                                                (map :commodity-id)
                                                                set)})
                                  (map (juxt :id
                                             #(:price (prices/most-recent % as-of))))
                                  (into {}))
                             {})]
      (->> lots
           (map #(lot-unrealized-gains % commodity-prices))
           (reduce + 0M)))))

(defn delete
  [lot]
  (with-storage (env :db)
    (storage/delete lot)))
