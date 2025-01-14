(ns clj-money.models.budgets
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.db :as db]
            [clj-money.models :as models]
            [clj-money.budgets :as budgets]))

(defn- all-accounts-belong-to-budget-entity?
  [{:budget/keys [entity items]}]
  (if (seq items)
    (when entity
      (when-let [accounts (->> items
                                  (map (comp :id :budget-item/account))
                                  (filter identity)
                                  seq)]
        (let [entities (->> (models/find-many accounts :account)
                            (map (comp :id :account/entity))
                            set)]
          (and (= 1 (count entities))
               (entities (:id entity))))))
    true))

(v/reg-spec all-accounts-belong-to-budget-entity?
            {:message "All accounts must belong to the budget entity"
             :path [:budget/items]})

(defn- period-counts-match?
  [{:budget/keys [items period-count]}]
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
(s/def :budget-item/spec (s/nilable map?))
(s/def ::models/budget-item (s/keys :req [:budget-item/account
                                          :budget-item/periods]
                                    :opt [:budget-item/spec]))

(s/def :budget/items (s/coll-of ::models/budget-item))
(s/def :budget/name v/non-empty-string?)
(s/def :budget/start-date t/local-date?)
(s/def :budget/period #{:week :month})
(s/def :budget/period-count v/positive-integer?)
(s/def :budget/entity ::models/model-ref)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/budget (s/and (s/keys :req [:budget/name
                                            :budget/start-date
                                            :budget/period
                                            :budget/period-count
                                            :budget/entity]
                                      :opt [:budget/items])
                              all-accounts-belong-to-budget-entity?
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
                           :entity entity}))

(defn update-items
  [{:budget/keys [items] :as budget}]
  (when (seq items)
    (let [existing (models/select {:budget-item/budget budget})
          current-ids (->> items
                           (map :id)
                           set)]
      (when-let [to-remove (seq (remove #(current-ids (:id %)) existing))]
        (models/delete-many to-remove))
      (models/put-many (map #(assoc % :budget-item/budget budget)
                            items)))))

(defn find-items-by-account
  "Finds items in the specified budget belonging to the specified account or its children."
  [{:budget/keys [items]} {:account/keys [child-ids] account-id :id}]
  (let [ids (if (seq child-ids)
              (conj (into #{} child-ids) account-id)
              (->> (models/select (db/model-type
                                    {:id account-id}
                                    :account)
                                  {:include-children? true})
                   (map :id)
                   (into #{})))]
    (filter #(ids (get-in % [:budget-item/account :id]))
            items)))
