(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET]]
            [environ.core :refer [env]]
            [clj-money.authorization :refer [+scope]]
            [clj-money.x-platform.util :refer [update-in-criteria
                                               parse-int
                                               unserialize-date]]
            [clj-money.api :refer [->response]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as transactions]
            [clj-money.authorization.transactions]))

(defn- prepare-criteria
  [params]
  (let [[start-date end-date]  (->> [:start-date :end-date]
                                    (map #(get-in params [%]))
                                    (map unserialize-date))
        result (select-keys params [:account-id :entity-id])]
    (cond
      (and start-date end-date)
      (assoc result :transaction-date [:between start-date end-date])

      start-date
      (assoc result :transaction-date [:>= start-date])

      end-date
      (assoc result :transaction-date [:<= end-date]))))

(defn- prepare-options
  [params]
  (-> params
      (select-keys [:limit :skip])
      (update-in-criteria :limit parse-int)
      (update-in-criteria :skip parse-int)
      (assoc :sort [[:transaction-date :desc]])))

(defn index
  [{:keys [params authenticated]}]
  {:pre [(or (:transaction-date params)
             (and (:start-date params)
                  (:end-date params)))]}
  (->response (transactions/search-items (env :db)
                                         (+scope (prepare-criteria params)
                                                      ::models/transaction-item
                                                      authenticated)
                                         (prepare-options params))))

(defroutes routes
  (GET "/api/accounts/:account-id/transaction-items" req (index req)))
