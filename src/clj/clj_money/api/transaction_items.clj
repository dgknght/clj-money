(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [compojure.core :refer [defroutes GET]]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-int
                                          parse-bool
                                          uuid]]
            [dgknght.app-lib.web :refer [unserialize-date ]]
            [dgknght.app-lib.authorization :refer [+scope]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :refer [presence]]
            [clj-money.transactions :refer [summarize-items]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.accounts :as acts]
            [clj-money.accounts :refer [->criteria]]
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
                                 {:id account-id}
                                 {:include-children? true})))
    criteria))

(defn- ensure-dates
  [{:keys [account-id] :as criteria}]
  (if (:transaction-date criteria)
    criteria
    (merge criteria (->criteria (acts/find account-id)))))

(defn- apply-unreconciled
  [{:keys [unreconciled] :as criteria}]
  (if (parse-bool unreconciled)
    (-> criteria
        (dissoc :unreconciled)
        (assoc :reconciliation-id nil))
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
      apply-unreconciled
      (select-keys [:transaction-date
                    :account-id
                    :entity-id
                    :reconciliation-id])
      (+scope ::models/transaction-item authenticated)))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      (update-in-if [:limit] parse-int)
      (update-in-if [:skip] parse-int)
      (assoc :sort [[:index :desc]])))

(defn- index
  [req]
  (api/response (transactions/search-items (extract-criteria req)
                                           (extract-options req))))

(defn- extract-summary-criteria
  [{:keys [params]}]
  (-> params
      (update-in-if [:interval-type] keyword)
      (update-in-if [:interval-count] parse-int)
      (update-in-if [:transaction-date 0] unserialize-date)
      (update-in-if [:transaction-date 1] unserialize-date))) ; TODO: Ensure start and end date

(defn- summarize
  [{:keys [authenticated] :as req}]
  (let [{[start-date end-date] :transaction-date
         :as criteria} (extract-summary-criteria req)
        items (transactions/search-items (-> criteria
                                             (update-in [:transaction-date] #(vec (cons :between %)))
                                             (select-keys [:transaction-date
                                                           :account-id])
                                             (+scope ::models/transaction-item authenticated)))]
    (api/response
      (summarize-items (-> criteria
                           (select-keys [:interval-type :interval-count])
                           (update-in [:interval-type] keyword)
                           (assoc :start-date start-date
                                  :end-date end-date))
                       items))))

(defroutes routes
  (GET "/api/accounts/:account-id/transaction-items" req (index req))
  (GET "/api/entities/:entity-id/transaction-items/summarize" req (summarize req)))
