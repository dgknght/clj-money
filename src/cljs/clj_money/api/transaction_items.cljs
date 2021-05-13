(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.api :as api]
            [clj-money.state :refer [current-entity]]
            [dgknght.app-lib.decimal :refer [->decimal]]))

(defn after-read
  [item]
  (-> item
      (update-in [:quantity] ->decimal)
      (update-in [:value] ->decimal)
      (update-in [:balance] ->decimal)
      (update-in [:transaction-date] unserialize-date)
      (update-in [:reconciliation-status] keyword)
      (update-in [:action] keyword)))

(defn- prepare-criteria
  [{:keys [transaction-date] :as criteria}]
  (-> criteria
      (assoc :start-date (serialize-date (nth transaction-date 1))
             :end-date (serialize-date (nth transaction-date 2)))
      (dissoc :transaction-date)))

(defn search
  [criteria success-fn error-fn]
  {:pre [(contains? criteria :account-id)]}

  (api/get (api/path :accounts
                     (:account-id criteria)
                     :transaction-items)
           (-> criteria
               (dissoc :account-id)
               prepare-criteria)
           (comp success-fn
                 #(map after-read %))
           error-fn))

(defn summarize
  [criteria success-fn error-fn]
  (api/get (str (api/path :entities
                          (:id @current-entity)
                          :transaction-items
                          :summarize)
                "?"
                (-> criteria
                    (update-in [:transaction-date 0] serialize-date)
                    (update-in [:transaction-date 1] serialize-date)
                    (update-in [:interval-type] name)
                    map->query-string))
           success-fn
           error-fn))
