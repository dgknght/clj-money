(ns clj-money.api.trading
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes POST]]
            [stowaway.core :as storage]
            [clj-money.api :refer [->response]]
            [clj-money.models :as models]
            [clj-money.authorization :refer [authorize] :as authorization]
            [clj-money.authorization.trades]
            [clj-money.trading :as trading]
            [clj-money.x-platform.util :refer [unserialize-date]]))

(def ^:private create-attributes
  [:trade-date :entity-id :shares :value :commodity-id :account-id])

(defn create
  [{:keys [params body authenticated]}]
  (let [f (if (= "sell" (:action body))
            trading/sell
            trading/buy)
        trade (-> params
                  (merge body)
                  (update-in [:trade-date] unserialize-date)
                  (select-keys create-attributes)
                  (storage/tag ::models/trade)
                  (authorize ::authorization/create authenticated))
        result (f (env :db) trade) ]
    (->response (select-keys result [:transaction :lot :lots]) 201)))

(defroutes routes
  (POST "/api/entities/:entity-id/trades" req (create req)))
