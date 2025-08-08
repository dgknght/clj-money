(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.models.schema :refer [strip]]
            [clj-money.api :as api :refer [add-error-handler]]
            [clj-money.state :refer [current-entity]]))

(defn select
  [criteria  & {:as opts}]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :budgets)
           criteria
           (add-error-handler
             opts
             "Unable to retrieve the budgets: %s")))

(defn find
  [id & {:as opts}]
  (api/get (api/path :budgets id)
           {}
           (add-error-handler
             opts
             "Unable to retrieve the budget: %s")))

(defn- update
  [budget {:as opts}]
  (api/patch
    (api/path :budgets
              (:id budget))
    (dissoc budget :id)
    (add-error-handler
      opts
      "Unable to update the budget: %s")))

(defn- create
  [budget {:as opts}]
  (api/post
    (api/path :entities
              (:id @current-entity)
              :budgets)
    budget
    (add-error-handler
      opts
      "Unable to create the budget: %s")))

(defn save
  [budget & {:as opts}]
  (let [f (if (:id budget)
            update
            create)]
    (f (strip budget :budget)
       opts)))

(defn delete
  [budget & {:as opts}]
  (api/delete (api/path :budgets (:id budget))
              (add-error-handler
                opts
                "Unable to delete the budget: %s")))
