(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.spec.alpha :as s]
            [dgknght.app-lib.core :refer [update-in-if
                                          parse-bool
                                          uuid
                                          index-by]]
            [dgknght.app-lib.api :as api]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.comparatives :as comparatives]
            [clj-money.transactions :refer [summarize-items
                                            polarize-item-quantity]]
            [clj-money.models :as models]
            [clj-money.accounts :refer [->criteria]]
            [clj-money.authorization :refer [+scope]]
            [clj-money.authorization.transactions]))

(defn- apply-account-recursion
  [{:keys [include-children] :as criteria}]
  (if (parse-bool include-children)
    (update-in criteria
               [:transaction-item/account]
               (fn [id]
                 [:in (map :id (models/select
                                 (util/model-type {:id id}
                                                  :account)
                                 {:include-children? true
                                  :select [:id]}))]))

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

(defn- unserialize-date
  [x]
  (cond
    (vector? x) (mapv unserialize-date x)
    (string? x) (dates/unserialize-local-date x)
    :else x))

(defn- extract-criteria
  [{:keys [params authenticated]}]
  {:pre [(or (:transaction-date params)
             (:account-id params))]}
  (-> params
      comparatives/symbolize
      (update-in-if [:transaction-date] unserialize-date)
      ensure-dates
      (rename-keys {:transaction-date :transaction/transaction-date
                    :account-id :transaction-item/account
                    :entity-id :transaction/entity
                    :reconciliation-id :transaction-item/reconciliation})
      apply-account-recursion
      (update-in-if [:transaction-item/account] util/->model-ref)
      (update-in-if [:transaction-item/reconciliation] (comp util/->model-ref
                                                             uuid))
      (update-in-if [:transaction/entity] util/->model-ref)
      (select-keys [:transaction/transaction-date
                    :transaction-item/account
                    :transaction-item/reconciliation
                    :transaction/entity])
      (+scope :transaction-item authenticated)))

(defn- extract-options
  [{:keys [params]}]
  (-> params
      (update-in-if [:limit] parse-long)
      (update-in-if [:skip] parse-long)
      (select-keys [#_:limit :skip])))

; TODO: fix the join problem with the query and move the limit back to the SQL query
(defn- apply-limit
  [{{:keys [limit]} :params} items]
  (if limit
    (take (parse-long limit) items)
    items))

(defn- polarize-quantities
  [items]
  (if (seq items)
    (let [accounts (index-by :id
                             (models/select
                               (util/model-type
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
                                 :sort [[:transaction/transaction-date :desc]
                                        [:transaction-item/index :desc]]
                                 :select-also [:transaction/description
                                               :transaction/transaction-date
                                               :transaction/attachment-count]
                                 :nil-replacements {:transaction/attachment-count 0})))
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

(s/def ::period-type #{"day" "week" "month" "year"})
(s/def ::period-count (partial re-matches #"^\d+$"))
(s/def ::raw-summary-options (s/keys :req-un [::period-type
                                              ::period-count]))

(defn- extract-summary-options
  [{{:keys [period-type period-count] :as params} :params}]
  {:pre [(s/valid? ::raw-summary-options params)]}
  {:period [(parse-long period-count)
            (keyword period-type)]})

(defn- summarize
  [{:keys [authenticated] :as req}]
  (let [{[_ since as-of] :transaction/transaction-date
         :as criteria} (extract-summary-criteria req)]
    (->> (models/select (+scope criteria
                                :transaction-item
                                authenticated)
                        {:select-also [:transaction/transaction-date]})
         polarize-quantities
         (summarize-items (assoc (extract-summary-options req)
                                 :since since
                                 :as-of as-of))
         api/response)))

(def routes
  [["accounts/:account-id/transaction-items" {:get {:handler index}}]
   ["entities/:entity-id/transaction-items/summarize" {:get {:handler summarize}}]])
