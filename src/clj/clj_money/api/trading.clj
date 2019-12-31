(ns clj-money.api.trading
  (:require [environ.core :refer [env]]
            [clj-money.api :refer [->response]]
            [clj-money.authorization :refer [tag-resource
                                             authorize]]
            [clj-money.permissions.trades]
            [clj-money.trading :as trading]
            [clj-money.x-platform.util :refer [unserialize-date]]))

(defn create
  [{:keys [params]}]
  (let [f (if (= "sell" (:action params))
            trading/sell
            trading/buy)
        trade (-> params
                  (update-in [:trade-date] unserialize-date)
                  (dissoc :action)
                  (tag-resource :trade)
                  (authorize :create))
        result (f (env :db) trade) ]
    (->response (select-keys result [:transaction :lot :lots]) 201)))
