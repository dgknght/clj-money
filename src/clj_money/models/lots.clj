(ns clj-money.models.lots
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [config.core :refer [env]]
            [java-time.api :as t]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(defn- asset-account?
  [account]
  (= :asset
     (:account/type (models/find account :account))))
(v/reg-msg asset-account? "%s must be an asset")

(s/def :lot/account (s/and ::models/model-ref
                           asset-account?))
(s/def :lot/commodity ::models/model-ref)
(s/def :lot/purchase-date t/local-date?)
(s/def :lot/purchase-price decimal?)
(s/def :lot/shares-purchased decimal?)
(s/def :lot/shares-owned decimal?)
(s/def ::models/lot (s/keys :req [:lot/account
                                  :lot/commodity
                                  :lot/purchase-date
                                  :lot/purchase-price
                                  :lot/shares-purchased]
                            :opt [:lot/shares-owned]))

(defn- after-read
  [lot]
  (-> lot
      (tag ::models/lot)
      (update-in [:purchase-date] t/local-date)))

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
      (update-in [:purchase-date] t/sql-date)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn create
  [lot]
  (with-storage (env :db)
    (with-validation lot ::new-lot
      (-> lot
          before-save
          storage/create
          after-read))))

(defn select-by-commodity-id
  [commodity-id]
  (if commodity-id
    (search {:commodity-id commodity-id})
    []))

(defn update
  [lot]
  (with-storage (env :db)
    (with-validation lot ::existing-lot
      (-> lot
          before-save
          storage/update)
      (find lot))))

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
