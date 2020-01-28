(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET]]
            [environ.core :refer [env]]
            [clj-money.authorization :refer [apply-scope]]
            [clj-money.x-platform.util :refer [update-in-criteria
                                               parse-int
                                               unserialize-date]]
            [clj-money.api :refer [->response]]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.transactions]))

(defn- prepare-criteria
  [params]
  (let [start-date (:start-date params)
        end-date (:end-date params) ]
    (cond-> (select-keys params [:account-id :entity-id])
      (and start-date end-date) (assoc :transaction-date [:between
                                                          (unserialize-date start-date)
                                                          (unserialize-date end-date)])
      start-date (assoc :transaction-date [:>= (unserialize-date start-date)])
      end-date (assoc :transaction-date [:<= (unserialize-date end-date)]))))

(defn- prepare-options
  [params]
  (-> params
      (select-keys [:limit :skip])
      (update-in-criteria :limit parse-int)
      (update-in-criteria :skip parse-int)))

(defn index
  [{:keys [params authenticated]}]
  {:pre [(or (:transaction-date params)
             (and (:start-date params)
                  (:end-date params)))]}
  (->response (transactions/search-items (env :db)
                                         (apply-scope (prepare-criteria params)
                                                      :transaction-item
                                                      authenticated)
                                         (prepare-options params))))

(defroutes routes
  (GET "/api/accounts/:account-id/transaction-items" req (index req)))
