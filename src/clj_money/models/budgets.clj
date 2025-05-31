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

(defn- account-belongs-to-budget-entity?
  [{:budget-item/keys [account budget]}]
  (if (and account budget)
    (let [account (models/resolve-ref account :account)
          budget (models/resolve-ref budget :budget)]
      (= (:entity account) (:entity budget)))
    true))

(v/reg-spec account-belongs-to-budget-entity?
            {:message "All accounts must belong to the budget entity"
             :path [:budget/items]})

(defn- period-counts-match?
  [{:budget/keys [items]
    [period-count] :budget/period}]
  (if (seq items)
    (let [item-period-counts (->> items
                                  (map (comp count :budget-item/periods))
                                  set)]
      (and (= 1 (count item-period-counts))
           (item-period-counts period-count)))
    true))

(v/reg-spec period-counts-match?
            {:message "All items must have a number of periods that matches the budget period count"
             :path [:budget/items]})

(s/def :budget-item/periods (s/coll-of decimal? :min-count 1 :kind vector?))
(s/def :budget-item/account ::models/model-ref)
(s/def :budget-item/spec (s/nilable ::budgets/item-spec))
(s/def :budget-item/budget ::models/model-ref)
(s/def ::models/budget-item (s/and
                              (s/keys :req [:budget-item/account
                                            :budget-item/periods
                                            :budget-item/budget]
                                      :opt [:budget-item/spec])
                              account-belongs-to-budget-entity?
                              period-counts-match?))

(s/def :budget/items (s/coll-of ::models/budget-item))
(s/def :budget/name v/non-empty-string?)
(s/def :budget/start-date t/local-date?)
(s/def :budget/period ::dates/period)
(s/def :budget/entity ::models/model-ref)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/budget (s/keys :req [:budget/name
                                     :budget/start-date
                                     :budget/period
                                     :budget/entity]
                               :opt [:budget/items]))

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
  [{:budget/keys [items]} {:account/keys [child-ids] account-id :id}]
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
