(ns clj-money.models.budget-items
  (:require [clojure.spec.alpha :as s]
            [clj-money.models :as models]
            [clj-money.budgets :as budgets]))

(s/def :budget-item/periods (s/coll-of decimal? :min-count 1 :kind vector?))
(s/def :budget-item/account ::models/model-ref)
(s/def :budget-item/spec (s/nilable ::budgets/item-spec))
(s/def ::models/budget-item (s/and
                              (s/keys :req [:budget-item/account
                                            :budget-item/periods]
                                      :opt [:budget-item/spec])))
