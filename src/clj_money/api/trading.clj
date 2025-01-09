(ns clj-money.api.trading
  (:require [dgknght.app-lib.api :as api]
            [clj-money.authorization :refer [authorize] :as authorization]
            [clj-money.dates :as dates]
            [clj-money.authorization.trades]
            [clj-money.trading :as trading]))

(def ^:private create-attributes
  [:trade/date
   :trade/entity
   :trade/shares
   :trade/value
   :trade/commodity
   :trade/account])

(def ^:private trade-fn-map
  {"buy" trading/buy
   "sell" trading/sell})

(defn- trade-fn
  [{:trade/keys [action]}]
  (or (trade-fn-map action)
      (throw (ex-info "Invalid trade action" {:action action}))))

(defn create
  [{:keys [params body authenticated]}]
  (let [f (trade-fn body)]
    (-> body
        (update-in [:trade/value] bigdec)
        (update-in [:trade/shares] bigdec)
        (update-in [:trade/date] dates/unserialize-local-date)
        (assoc :trade/entity {:id (:entity-id params)})
        (select-keys create-attributes)
        (authorize ::authorization/create authenticated)
        f
        (select-keys [:trade/transaction
                      :trade/lot
                      :trade/lots])
        api/creation-response)))

(def routes
  ["entities/:entity-id/trades" {:post {:handler create}}])
