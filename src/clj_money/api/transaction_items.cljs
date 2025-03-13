(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [lambdaisland.uri :refer [map->query-string]]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (dissoc :transaction-item/account)
      (update-in [:transaction-item/transaction-date] #(map serialize-local-date %))))

(defn select
  [criteria & {:as opts}]
  {:pre [(:transaction-item/account criteria)
         (:transaction-item/transaction-date criteria)]}

  (api/get (api/path :accounts
                     (:transaction-item/account criteria)
                     :transaction-items)
           (prepare-criteria criteria)
           (add-error-handler opts "Unable to retrieve the transaction items: %s")))

(defn- prepare-summary-criteria
  [criteria]
  (-> criteria
      (update-in [:transaction-date 0] serialize-local-date)
      (update-in [:transaction-date 1] serialize-local-date)
      (update-in [:interval-type] name)
      map->query-string))

(defn summarize
  [criteria & {:as opts}]
  (api/get (str (api/path :entities
                          @current-entity
                          :transaction-items
                          :summarize)
                "?"
                (prepare-summary-criteria criteria))
           (add-error-handler opts "Unable to retrieve the transaction item summary: %s")))
