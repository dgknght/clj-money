(ns clj-money.entities.lots
  (:require [clojure.spec.alpha :as s]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.entities :as entities]
            [clj-money.entities.commodities :as commodities]
            [clj-money.entities.propagation :as prop]))

(defn- asset-account?
  [account]
  (= :asset
     (:account/type (entities/resolve-ref account))))
(v/reg-msg asset-account? "%s must be an asset")

(s/def :lot/account (s/and ::entities/entity-ref
                           asset-account?))
(s/def :lot/commodity ::entities/entity-ref)
(s/def :lot/purchase-date t/local-date?)
(s/def :lot/purchase-price decimal?)
(s/def :lot/shares-purchased decimal?)
(s/def :lot/shares-owned decimal?)
(s/def ::entities/lot (s/keys :req [:lot/account
                                  :lot/commodity
                                  :lot/purchase-date
                                  :lot/purchase-price
                                  :lot/shares-purchased]
                            :opt [:lot/shares-owned]))

(defmethod prop/propagate :lot
  [[before after]]
  (when-let [commodity (some-> (or after before)
                               :lot/commodity
                               (entities/find))]
    [(commodities/with-shares-owned commodity)]))
