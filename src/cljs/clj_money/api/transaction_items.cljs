(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.api-async :as api]
            [dgknght.app-lib.decimal :refer [->decimal]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :refer [handle-ex]]))

(defn after-read
  [item]
  (-> item
      (update-in [:quantity] ->decimal)
      (update-in [:value] ->decimal)
      (update-in [:balance] ->decimal)
      (update-in [:transaction-date] unserialize-date)
      (update-in [:reconciliation-status] keyword)
      (update-in [:action] keyword)))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (dissoc :account-id)
      (update-in [:transaction-date] #(map serialize-date %))))

(defn search
  [criteria xf]
  {:pre [(:account-id criteria)
         (:transaction-date criteria)]}

  (api/get (api/path :accounts
                     (:account-id criteria)
                     :transaction-items)
           (prepare-criteria criteria)
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the transaction items: %s")}))

(defn summarize
  [criteria xf]
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
           {:transform xf
            :handle-ex (handle-ex "Unable to retrieve the transaction item summary: %s")}))
