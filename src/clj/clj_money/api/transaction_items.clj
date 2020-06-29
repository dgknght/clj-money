(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET]]
            [environ.core :refer [env]]
            [clj-money.authorization :refer [+scope]]
            [clj-money.util :refer [uuid
                                    presence]]
            [clj-money.x-platform.util :refer [update-in-if
                                               parse-int
                                               parse-bool
                                               unserialize-date]]
            [clj-money.api :refer [->response]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.accounts :as acts]
            [clj-money.x-platform.accounts :refer [->criteria]]
            [clj-money.authorization.transactions]))

(defn- translate-dates
  [criteria]
  (let [[start-date end-date] (map (comp unserialize-date
                                         #(get-in criteria [%]))
                                   [:start-date :end-date])]
    (-> criteria
        (dissoc :start-date :end-date)
        (assoc :transaction-date (cond
                                   (and start-date end-date)
                                   [:between start-date end-date]

                                   start-date
                                   [:>= start-date]

                                   end-date
                                   [:<= end-date])))))

(defn- apply-child-inclusion
  [{:keys [account-id] :as criteria} include-children?]
  (if include-children?
    (merge criteria (->criteria (acts/search
                                  (env :db)
                                  {:id account-id}
                                  {:include-children? true})))
    criteria))

(defn- ensure-dates
  [{:keys [account-id] :as criteria}]
  (if (:transaction-date criteria)
    criteria
    (merge criteria (->criteria (acts/find-by-id (env :db) account-id)))))

(defn- apply-unreconciled
  [{:keys [unreconciled] :as criteria}]
  (if (parse-bool unreconciled)
    (-> criteria
        (dissoc :unreconciled)
        (assoc [:reconciliation :status] [:or :new nil]))
    criteria))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  {:pre [(or (:transaction-date params)
             (and (:start-date params)
                  (:end-date params))
             (:account-id params))]}
  (-> params
      translate-dates
      (apply-child-inclusion (parse-bool (:include-children params)))
      ensure-dates
      (update-in-if [:reconciliation-id] (comp uuid presence))
      (select-keys [:transaction-date
                    :account-id
                    :entity-id
                    :reconciliation-id
                    :unreconciled])
      apply-unreconciled
      (+scope ::models/transaction-item authenticated)))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      (update-in-if [:limit] parse-int)
      (update-in-if [:skip] parse-int)
      (assoc :sort [[:transaction-date :desc]])))

(defn index
  [req]
  (->response (transactions/search-items (env :db)
                                         (extract-criteria req)
                                         (extract-options req))))

(defroutes routes
  (GET "/api/accounts/:account-id/transaction-items" req (index req)))
