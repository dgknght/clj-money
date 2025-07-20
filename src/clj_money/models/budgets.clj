(ns clj-money.models.budgets
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.budgets :as budgets]))

(defn- period-counts-match?
  [{:budget/keys [items period]}]
  (every? #(= (first period)
              (count (:budget-item/periods %)))
          items))

(v/reg-spec period-counts-match?
            {:message "Must have the number of periods specified by the budget"
             :path [:budget-item/periods]})

(defn- accounts-belong-to-budget-entity?
  [{:budget/keys [entity items]}]
  (every? #(util/id= entity
                     (-> %
                         :budget-item/account
                         (models/resolve-ref :account)
                         :account/entity))
          items))

(v/reg-spec accounts-belong-to-budget-entity?
            {:message "Accounts must belong to the same entity as the budget"
             :path [:budget/items]})

(s/def :budget/items (s/coll-of ::models/budget-item))
(s/def :budget/name v/non-empty-string?)
(s/def :budget/start-date t/local-date?)
(s/def :budget/period ::dates/period)
(s/def :budget/entity ::models/model-ref)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/budget (s/and (s/keys :req [:budget/name
                                            :budget/start-date
                                            :budget/period
                                            :budget/entity]
                                      :opt [:budget/items])
                              accounts-belong-to-budget-entity?
                              period-counts-match?))

(defmethod models/before-save :budget
  [budget]
  (assoc budget :budget/end-date (budgets/end-date budget)))

(defn find-by-date
  "Returns the budget containing the specified date"
  [entity date]
  {:pre [entity
         (:id entity)
         date
         (t/local-date? date)]}
  (models/find-by #:budget{:start-date [:<= date]
                           :end-date [:>= date]
                           :entity entity}
                  {:include #{:budget/items}}))

(defn find-items-by-account
  "Finds items in the specified budget belonging to the specified account or its children."
  [items {:account/keys [child-ids] account-id :id}]
  {:pre [(seq items)]}
  (let [ids (if (seq child-ids)
              (conj (into #{} child-ids) account-id)
              (->> (models/select (util/model-type
                                    {:id account-id}
                                    :account)
                                  {:include-children? true})
                   (map :id)
                   (into #{})))]
    (filter #(ids (get-in % [:budget-item/account :id]))
            items)))
