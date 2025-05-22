(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [cljs.pprint :refer [pprint]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.comparatives :as comparatives]
            [clj-money.dates :refer [serialize-local-date
                                     local-date?]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- serialize-date
  [x]
  (cond
    (vector? x)     (mapv serialize-date x)
    (local-date? x) (serialize-local-date x)
    :else           x))

(defn- prepare-criteria
  [criteria]
  {:pre [(:transaction-item/transaction-date criteria)]}
  (-> criteria
      (update-in [:transaction-item/transaction-date] serialize-date)
      comparatives/nominalize
      (dissoc :transaction-item/account)))

(defn select
  [criteria & {:as opts}]
  {:pre [(:transaction-item/account criteria)]}
  (api/get (api/path :accounts
                     (:transaction-item/account criteria)
                     :transaction-items)
           (prepare-criteria criteria)
           (add-error-handler opts "Unable to retrieve the transaction items: %s")))

(defn- prepare-summary-criteria
  [{:keys [period] :as criteria}]
  (-> criteria
      (dissoc :period)
      (update-in [:transaction-item/transaction-date 0] serialize-local-date)
      (update-in [:transaction-item/transaction-date 1] serialize-local-date)
      (update-in-if [:transaction-item/account] :id)
      (rename-keys {:transaction-item/transaction-date :transaction-date
                    :transaction-item/account :account-id})
      (assoc :period-type (name (second period)))
      (assoc :period-count (first period))
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
