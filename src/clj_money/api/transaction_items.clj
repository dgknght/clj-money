(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
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

(defn- apply-account-recursion
  [{:keys [include-children] :as criteria}]
  (if (parse-bool include-children)
    (update-in criteria
               [:transaction-item/account]
               (fn [id]
                 [:in (mapv :id (models/select
                                  (db/model-type {:id id} :account)
                                  {:include-children? true
                                   :select :account/id}))]))

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
      ensure-dates
      (rename-keys {:transaction-date :transaction-item/transaction-date
                    :account-id :transaction-item/account
                    :entity-id :transaction/entity
                    :reconciliation-id :transaction-item/reconciliation})
      apply-account-recursion
      (update-in-if [:transaction-item/account] util/->model-ref)
      (update-in-if [:transaction-item/reconciliation] (comp util/->model-ref
                                                             uuid))
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
                                 :sort [[:transaction-item/transaction-date :desc]
                                        [:transaction-item/index :desc]]
                                 :select-also [:transaction/description
                                               :transaction/attachment-count])))
       (filter-reconciled req)
       (apply-limit req)
       polarize-quantities
       ->model-refs
       api/response))

(s/def ::serialized-date (partial re-matches #"^\d{4}-\d{2}-\d{2}$"))
(s/def ::transaction-date (s/coll-of ::serialized-date :count 2))
(s/def ::account-id int?)
(s/def ::entity-id int?)
(s/def ::raw-summary-criteria (s/keys :req-un [::transaction-date]
                                      :opt-un [::account-id
                                               ::entity-id]))

(defn- extract-summary-criteria
  [{:keys [params] :as req}]
  {:pre [(s/valid? ::raw-summary-criteria (:params req))]}
  (-> params
      (rename-keys {:transaction-date :transaction/transaction-date
                    :account-id :transaction-item/account
                    :entity-id :transaction/entity})
      (update-in [:transaction/transaction-date]
                 (fn [dates]
                   (apply vector
                          :between>
                          (map dates/unserialize-local-date dates))))
      (update-in-if [:transaction-item/account] util/->model-ref)
      (update-in-if [:transaction/entity] util/->model-ref)
      (select-keys [:transaction-item/account
                    :transaction/transaction-date
                    :transaction/entity])))

(s/def ::interval-type #{"day" "week" "month" "year"})
(s/def ::interval-count (partial re-matches #"^\d+$"))
(s/def ::raw-summary-options (s/keys :req-un [::interval-type
                                              ::interval-count]))

(defn- extract-summary-options
  [{:keys [params]}]
  {:pre [(s/valid? ::raw-summary-options params)]}
  (-> params
      (update-in [:interval-type] keyword)
      (update-in [:interval-count] parse-int)
      (select-keys [:interval-type :interval-count])))

(defn- summarize
  [{:keys [authenticated] :as req}]
  (let [{[_ since as-of] :transaction/transaction-date
         :as criteria} (extract-summary-criteria req)]
    (->> (models/select (+scope criteria
                                :transaction-item
                                authenticated))
         polarize-quantities
         (summarize-items (assoc (extract-summary-options req)
                                 :since since
                                 :as-of as-of))
         api/response)))

(def routes
  [["accounts/:account-id/transaction-items" {:get {:handler index}}]
   ["entities/:entity-id/transaction-items/summarize" {:get {:handler summarize}}]])
