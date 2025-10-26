(ns clj-money.entities.budget-items
  (:require [clojure.spec.alpha :as s]
            [clj-money.entities :as entities]
            [clj-money.budgets :as budgets]))

(s/def :budget-item/periods (s/coll-of decimal? :min-count 1 :kind vector?))
(s/def :budget-item/account ::entities/entity-ref)
(s/def :budget-item/spec (s/nilable ::budgets/item-spec))
(s/def ::entities/budget-item (s/keys :req [:budget-item/account
                                          :budget-item/periods]
                                    :opt [:budget-item/spec]))
