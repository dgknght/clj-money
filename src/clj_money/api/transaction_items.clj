(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-int
                                          parse-bool
                                          uuid
                                          index-by]]
            [dgknght.app-lib.api :as api]
            [clj-money.db :as db]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.transactions :refer [summarize-items
                                            polarize-item-quantity]]
            [clj-money.models :as models]
            [clj-money.accounts :refer [->criteria]]
            [clj-money.authorization :refer [+scope]]
            [clj-money.authorization.transactions]))

(defn- translate-dates
  [criteria]
  (update-in-if criteria
                [:transaction-date]
                (fn [v]
                  (if (vector? v)
                    [:between>
                     (dates/unserialize-local-date (first v))
                     (dates/unserialize-local-date (second v))]
                    (dates/unserialize-local-date v)))))

(defn- apply-child-inclusion
  [{:keys [account-id] :as criteria} include-children?]
  (if include-children?
    (merge criteria (->criteria (models/select
                                 (db/model-type {:id account-id}
                                                :account)
                                 {:include-children? true})))
    criteria))

(defn- ensure-dates
  [{:keys [account-id] :as criteria}]
  (if (:transaction-date criteria)
    criteria
    (merge criteria (->criteria (models/find account-id :account)))))

; This could be done at the database layer with more sophisticated
; logic for specifying joins
(defn- filter-reconciled
  [{{:keys [unreconciled]} :params} items]
  (if (parse-bool unreconciled)
    (if-let [recon-ids (->> items
                            (map (comp :id :transaction-item/reconciliation))
                            set
                            seq)]
      (let [unreconciled? (complement
                            (->> (models/select
                                   {:id [:in recon-ids]
                                    :reconciliation/status :completed})
                                 (map :id)
                                 set))]
        (filter (comp unreconciled?
                      :id
                      :transaction-item/reconciliation)
                items))
      items)
    items))

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
      (update-in-if [:reconciliation-id] (comp uuid util/presence))
      (select-keys [:transaction-date
                    :account-id
                    :entity-id
                    :unreconciled
                    :reconciliation-id])
      (rename-keys {:transaction-date :transaction-item/transaction-date
                    :account-id :transaction-item/account
                    :entity-id :transaction/entity
                    :reconciliation-id :transaction-item/reconciliation})
      (update-in-if [:transaction-item/account] util/->model-ref)
      (update-in-if [:transaction-item/reconciliation] util/->model-ref)
      (update-in-if [:transaction/entity] util/->model-ref)
      (select-keys [:transaction-item/transaction-date
                    :transaction-item/account
                    :transaction-item/reconciliation
                    :transaction/entity])
      (+scope :transaction-item authenticated)))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      #_(update-in-if [:limit] parse-int)
      (update-in-if [:skip] parse-int)
      (select-keys [#_:limit :skip])))

; TODO: fix the join problem with the query and move the limit back to the SQL query
(defn- apply-limit
  [{{:keys [limit]} :params} items]
  (if limit
    (take (parse-int limit) items)
    items))

(defn- polarize-quantities
  [items]
  (if (seq items)
    (let [accounts (index-by :id
                             (models/select
                               (db/model-type
                                 {:id [:in (set (map (comp :id :transaction-item/account)
                                                     items))]}
                                 :account)))]
      (map (comp polarize-item-quantity
                 #(update-in %
                             [:transaction-item/account]
                             (comp accounts :id)))
           items))
    items))

(defn- ->model-refs
  [items]
  (map #(update-in %
                   [:transaction-item/account]
                   util/->model-ref)
       items))

(defn- index
  [req]
  (->> (-> req
           extract-criteria
           (models/select (assoc (extract-options req)
                                 :sort [[:transaction-item/index :desc]]
                                 :select-also [:transaction/description])))
       (filter-reconciled req)
       (apply-limit req)
       polarize-quantities
       ->model-refs
       api/response))

(defn- extract-summary-criteria
  [{:keys [params]}]
  (-> params
      (update-in-if [:interval-type] keyword)
      (update-in-if [:interval-count] parse-int)
      (update-in-if [:transaction-date 0] dates/unserialize-local-date)
      (update-in-if [:transaction-date 1] dates/unserialize-local-date))) ; TODO: Ensure start and end date

(defn- summarize
  [{:keys [authenticated] :as req}]
  (let [{[start-date end-date] :transaction-date
         :as criteria} (extract-summary-criteria req)
        items (models/select (-> criteria
                                 (update-in [:transaction-item/transaction-date] #(apply vector :between> %))
                                 (select-keys [:transaction-date/transaction-date
                                               :transaction-date/account])
                                 (+scope :transaction-item authenticated)))]
    (api/response
      (summarize-items (-> criteria
                           (select-keys [:interval-type :interval-count])
                           (update-in [:interval-type] keyword)
                           (assoc :start-date start-date
                                  :end-date end-date))
                       items))))

(def routes
  [["accounts/:account-id/transaction-items" {:get {:handler index}}]
   ["entities/:entity-id/transaction-items/summarize" {:get {:handler summarize}}]])
