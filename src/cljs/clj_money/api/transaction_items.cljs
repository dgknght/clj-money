(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]
            [clj-money.state :refer [current-entity]]
            [clj-money.decimal :refer [->decimal]]
            [clj-money.util :refer [serialize-date
                                    unserialize-date
                                    map->query-string]]))

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

  (api/get-resources (api/path :accounts
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
  (api/get-resources (str (api/path :entities
                               (:id @current-entity)
                               :transaction-items
                               :summarize)
                          "?"
                          (-> criteria
                              (update-in [:transaction-date 0] serialize-date)
                              (update-in [:transaction-date 1] serialize-date)
                              map->query-string))
                     success-fn
                     error-fn))
