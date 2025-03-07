(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.web :refer [serialize-date]]
            [lambdaisland.uri :refer [map->query-string]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (dissoc :account-id)
      (update-in [:transaction-date] #(map serialize-date %))))

(defn search
  [criteria & {:as opts}]
  {:pre [(:account-id criteria)
         (:transaction-date criteria)]}

  (api/get (api/path :accounts
                     (:account-id criteria)
                     :transaction-items)
           (prepare-criteria criteria)
           (add-error-handler opts "Unable to retrieve the transaction items: %s")))

(defn summarize
  [criteria & {:as opts}]
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
           (add-error-handler opts "Unable to retrieve the transaction item summary: %s")))
