(ns clj-money.api.trading
  (:require [stowaway.core :as storage]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
            [dgknght.app-lib.authorization :refer [authorize] :as authorization]
            [clj-money.authorization.trades]
            [clj-money.trading :as trading]))

(def ^:private create-attributes
  [:trade-date :entity-id :shares :value :commodity-id :account-id])

(defn create
  [{:keys [params authenticated]}]
  (let [f (if (= :sell (:action params))
            trading/sell
            trading/buy)]
    (api/creation-response
      (-> params
          (select-keys create-attributes)
          (storage/tag ::models/trade)
          (authorize ::authorization/create authenticated)
          f
          (select-keys [:transaction :lot :lots])))))

(def routes
  ["entities/:entity-id/trades" {:post {:handler create}}])
