(ns clj-money.models.budget-items
  (:require [clojure.spec.alpha :as s]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :refer [id=]]
            [clj-money.models :as models]
            [clj-money.budgets :as budgets]))

(defn- account-belongs-to-budget-entity?
  [{:budget-item/keys [account budget]}]
  (id= (:account/entity (models/resolve-ref account :account))
       (:budget/entity (models/resolve-ref budget :budget))))

(v/reg-spec account-belongs-to-budget-entity?
            {:message "Account must belong to the same entity as the budget"
             :path [:budget-item/account]})

(defn- period-counts-match?
  [{:budget-item/keys [periods budget]}]
  (let [{:budget/keys [period]} (models/resolve-ref budget :budget)]
    (= (count periods)
       (first period))))

(v/reg-spec period-counts-match?
            {:message "Must have the number of periods specified by the budget"
             :path [:budget-item/periods]})

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
