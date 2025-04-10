(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [cljs.pprint :refer [pprint]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [cljs-time.core :as t]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(def ^:private transaction-date
  (some-fn :transaction/transaction-date
           :transaction-item/transaction-date))

(defn- prepare-criteria
  [criteria]
  (let [criterion (transaction-date criteria)
        [start end] (if (= 2 (count criterion))
                      criterion
                      (rest criterion))]
    (-> criteria
        (dissoc :transaction-item/account
                :transaction-item/transaction-date
                :transaction/transaction-date)
        (assoc :transaction-item/transaction-date [(serialize-local-date start)
                                                   (serialize-local-date (t/plus end (t/days 1)))]))))

(defn select
  [criteria & {:as opts}]
  {:pre [(:transaction-item/account criteria)
         (transaction-date criteria)]}

  (api/get (api/path :accounts
                     (:transaction-item/account criteria)
                     :transaction-items)
           (prepare-criteria criteria)
           (add-error-handler opts "Unable to retrieve the transaction items: %s")))

(defn- prepare-summary-criteria
  [criteria]
  (-> criteria
      (update-in [:transaction-item/transaction-date 0] serialize-local-date)
      (update-in [:transaction-item/transaction-date 1] serialize-local-date)
      (update-in-if [:transaction-item/account] :id)
      (rename-keys {:transaction-item/transaction-date :transaction-date
                    :transaction-item/account :account-id})
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
