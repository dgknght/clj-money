(ns clj-money.entities.budgets
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.entities :as entities]
            [clj-money.budgets :as budgets]))

(defn- accounts-belong-to-budget-entity?
  [{:budget/keys [items entity]}]
  (->> items
       (map :budget-item/account)
       set
       (map #(entities/resolve-ref % :account))
       (every? #(id= (:account/entity %)
                     entity))))

(v/reg-spec accounts-belong-to-budget-entity?
            {:message "Account must belong to the same entity as the budget"
             :path [:budget/items]})

(defn- period-counts-match?
  [{:budget/keys [period items]}]
  (every? #(= (count (:budget-item/periods %))
              (first period))
          items))

(v/reg-spec period-counts-match?
            {:message "Must have the number of periods specified by the budget"
             :path [:budget-item/periods]})

(s/def :budget/items (s/nilable (s/coll-of ::entities/budget-item)))
(s/def :budget/name v/non-empty-string?)
(s/def :budget/start-date t/local-date?)
(s/def :budget/period ::dates/period)
(s/def :budget/entity ::entities/entity-ref)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/budget (s/and (s/keys :req [:budget/name
                                            :budget/start-date
                                            :budget/period
                                            :budget/entity]
                                      :opt [:budget/items])
                              accounts-belong-to-budget-entity?
                              period-counts-match?))

(defmethod entities/before-save :budget
  [budget]
  (assoc budget :budget/end-date (budgets/end-date budget)))

(defn find-by-date
  "Returns the budget containing the specified date"
  [entity date]
  {:pre [entity
         (:id entity)
         date
         (t/local-date? date)]}
  (entities/find-by #:budget{:start-date [:<= date]
                           :end-date [:>= date]
                           :entity entity}
                  {:include #{:budget/items}}))

(defn find-items-by-account
  "Finds items in the specified budget belonging to the specified account or its children."
  [items {:account/keys [child-ids] account-id :id}]
  {:pre [(seq items)]}
  (let [ids (if (seq child-ids)
              (conj (into #{} child-ids) account-id)
              (->> (entities/select (util/entity-type
                                    {:id account-id}
                                    :account)
                                  {:include-children? true})
                   (map :id)
                   (into #{})))]
    (filter #(ids (get-in % [:budget-item/account :id]))
            items)))
