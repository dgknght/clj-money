(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [clj-money.api :as api :refer [handle-ex]]
            [clj-money.state :refer [current-entity]]))

(defn search
  [xf]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :budgets)
           {}
           {:post-xf xf
            :handle-ex (handle-ex "Unable to retrieve the budgets: %s")}))

(defn find
  [id xf]
  (api/get (api/path :budgets id)
           {}
           {:post-xf xf
            :handle-ex (handle-ex "Unable to retrieve the budget: %s")}))

(defn- before-item-save
  [item]
  (update-in-if item [:spec :start-date] serialize-date))

(defn- before-save
  [budget]
  (-> budget
      (dissoc :end-date)
      (update-in [:start-date] serialize-date)
      (update-in-if [:items] #(map before-item-save %))))

(defn- update
  [budget xf]
  (api/patch
    (api/path :budgets
              (:id budget))
    budget
    {:post-xf xf}))

(defn- create
  [budget xf]
  (api/post
    (api/path :entities
              (:id @current-entity)
              :budgets)
    (update-in-if budget [:auto-create-start-date] serialize-date)
    {:post-xf xf
     :handle-ex (handle-ex "Unable to create the budget: %s")}))

(defn save
  [budget xf]
  (let [f (if (:id budget)
            update
            create)]
    (f (before-save budget) xf)))

(defn delete
  [budget xf]
  (api/delete (api/path :budgets (:id budget))
              {:post-xf xf
               :handle-ex (handle-ex "Unable to remove the budget: %s")}))
