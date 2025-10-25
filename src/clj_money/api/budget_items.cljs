(ns clj-money.api.budget-items
  (:refer-clojure :exclude [update])
  (:require [clj-money.entities.schema :as schema]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  [{:as item :budget-item/keys [budget]} opts]
  {:pre [(:budget-item/budget item)]}
  (api/post
    (api/path :budgets
              budget
              :items)
    (dissoc item :budget-item/budget)
    (add-error-handler
      opts
      "Unable to create the budget item: %s")))

(defn update
  [item opts]
  (api/patch
    (api/path :budget-items item)
    (dissoc item :budget-item/budget)
    (add-error-handler
      opts
      "Unable to update the budget item: %s")))

(defn save
  [item & {:as opts}]
  (let [f (if (:id item)
            update
            create)]
    (-> item
        (schema/prune :budget-item)
        (f opts))))
