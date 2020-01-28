(ns clj-money.api.prices
  (:require [environ.core :refer [env]]
            [compojure.core :refer [defroutes GET]]
            [clj-money.authorization :refer [apply-scope]]
            [clj-money.api :refer [->response]]
            [clj-money.x-platform.util :refer [unserialize-date
                                               update-in-criteria]]
            [clj-money.models.prices :as prices]
            [clj-money.permissions.prices]))

(defn index
  [{:keys [params authenticated]}]
  (->response (prices/search (env :db) (-> (:criteria params)
                                           (merge (dissoc params :criteria))
                                           (update-in-criteria :trade-date unserialize-date)
                                           (select-keys [:commodity-id :entity-id :trade-date])
                                           (apply-scope :price authenticated)))))

(defroutes routes
  (GET "/api/commodities/:commodity-id/prices" req (index req)))
