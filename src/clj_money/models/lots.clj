(ns clj-money.models.lots
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.db :as db]
            [clj-money.models :as models]
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

(defn- lot-unrealized-gains
  [{:lot/keys [purchase-price
               commodity
               shares-owned]}
   price-map]
  (let [cost (* purchase-price shares-owned)
        price (or (price-map (:id commodity))
                  0M)
        value (* price shares-owned)]
    (when (= 0M price)
      (log/errorf "Unable to find price for commodity %s to calculate unrealized gains" commodity))
    (- value cost)))

(defn unrealized-gains
  [entity as-of]
  (let [lots (models/select
               (db/model-type
                 {:commodity/entity entity
                  :lot/purchase-date [:<= as-of]}
                 :lot))
        commodity-prices (if (seq lots)
                           (->> (models/select
                                  (db/model-type
                                    {:id [:in (->> lots
                                                   (map (comp :id :lot/commodity))
                                                   set)]}
                                    :commodity))
                                (map (juxt :id
                                           #(:price/price (prices/most-recent % as-of))))
                                (into {}))
                           {})]
    (->> lots
         (map #(lot-unrealized-gains % commodity-prices))
         (reduce + 0M))))
