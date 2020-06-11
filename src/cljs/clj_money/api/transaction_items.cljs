(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]
            [clj-money.decimal :refer [->decimal]]
            [clj-money.x-platform.util :refer [serialize-date unserialize-date]]))

(defn- after-read
  [item]
  (-> item
      (update-in [:quantity] ->decimal)
      (update-in [:value] ->decimal)
      (update-in [:balance] ->decimal)
      (update-in [:transaction-date] unserialize-date)
      (update-in [:action] keyword)))

(defn- prepare-criteria
  [{:keys [transaction-date] :as criteria}]
  (-> criteria
      (assoc :start-date (serialize-date (nth transaction-date 1))
             :end-date (serialize-date (nth transaction-date 2)))
      (dissoc :transaction-date)))

(defn search
  [criteria success-fn error-fn]
  {:pre [(every? #(contains? criteria %) [:account-id :transaction-date])]}

  (api/get-resources (api/path :accounts
                               (:account-id criteria)
                               :transaction-items)
                     (-> criteria
                         (dissoc :account-id)
                         prepare-criteria)
                     (comp success-fn
                           #(map after-read %))
                     error-fn))
