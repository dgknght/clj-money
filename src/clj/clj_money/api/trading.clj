(ns clj-money.api.trading
  (:require [compojure.core :refer [defroutes POST]]
            [stowaway.core :as storage]
            [dgknght.app-lib.web :refer [unserialize-date]]
            [dgknght.app-lib.api :as api]
            [clj-money.models :as models]
            [dgknght.app-lib.authorization :refer [authorize] :as authorization]
            [clj-money.authorization.trades]
            [clj-money.trading :as trading]))

(def ^:private create-attributes
  [:trade-date :entity-id :shares :value :commodity-id :account-id])

(defn create
  [{:keys [params body authenticated]}]
  (let [f (if (= "sell" (:action body))
            trading/sell
            trading/buy)]
    (api/creation-response
      (-> params
          (merge (-> body
                     (update-in [:value] bigdec)
                     (update-in [:shares] bigdec)
                     (update-in [:trade-date] unserialize-date)))
          (select-keys create-attributes)
          (storage/tag ::models/trade)
          (authorize ::authorization/create authenticated)
          f
          (select-keys [:transaction :lot :lots])))))

(defroutes routes
  (POST "/api/entities/:entity-id/trades" req (create req)))
