(ns clj-money.api.trading
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.authorization :refer [authorize] :as authorization]
            [clj-money.authorization.trades]
            [clj-money.trading :as trading]
            [clj-money.dates :as dates]))

(def ^:private create-attributes
  [:trade/date
   :trade/entity
   :trade/shares
   :trade/value
   :trade/commodity
   :trade/account
   :trade/dividend?
   :trade/dividend-account])

(def ^:private trade-fn-map
  {:buy (comp first trading/buy-and-propagate)
   :sell (comp first trading/sell-and-propagate)})

(defn- trade-fn
  [{:trade/keys [action]}]
  (or (trade-fn-map (util/ensure-keyword action))
      (throw (ex-info "Invalid trade action" {:action action}))))

(defn- extract-trade
  [{:keys [params]}]
  (-> params
      (assoc :trade/entity {:id (:entity-id params)})
      (select-keys create-attributes)
      (update-in-if [:trade/shares] bigdec)
      (update-in-if [:trade/value] bigdec)
      (update-in-if [:trade/date] dates/ensure-local-date)))

(defn create
  [{:keys [params authenticated] :as req}]
  (let [f (trade-fn params)]
    (-> (extract-trade req)
        (authorize ::authorization/create authenticated)
        f
        (select-keys [:trade/transactions
                      :trade/lot
                      :trade/lots])
        (update-in [:trade/lot :lot/account] util/->entity-ref)
        (update-in [:trade/lot :lot/commodity] util/->entity-ref)
        api/creation-response)))

(def routes
  ["entities/:entity-id/trades" {:post {:handler create}}])
