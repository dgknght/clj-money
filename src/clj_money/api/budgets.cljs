(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [clojure.walk :refer [keywordize-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date]]
            [dgknght.app-lib.api-async :as api]
            [dgknght.app-lib.decimal :refer [->decimal]]

            [clj-money.api :refer [handle-ex]]
            [clj-money.state :refer [current-entity]]))

(defn- after-item-read
  [item]
  (-> item
      (update-in [:periods] #(mapv ->decimal %))
      (update-in-if [:spec] keywordize-keys)
      (update-in-if [:spec :start-date] unserialize-date)
      (update-in-if [:spec :entry-mode] keyword)
      (update-in-if [:spec :amount-per] ->decimal)))

(defn- after-read
  [budget]
  (-> budget
      (update-in-if [:items] #(map after-item-read %))
      (update-in [:period] keyword)
      (update-in [:start-date] unserialize-date)
      (update-in [:end-date] unserialize-date)))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn search
  [xf]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :budgets)
           {}
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the budgets: %s")}))

(defn find
  [id xf]
  (api/get (api/path :budgets id)
           {}
           {:transform (transform xf)
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
    {:transform (transform xf)}))

(defn- create
  [budget xf]
  (api/post
    (api/path :entities
              (:id @current-entity)
              :budgets)
    (update-in-if budget [:auto-create-start-date] serialize-date)
    {:transform (transform xf)
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
              {:transform xf
               :handle-ex (handle-ex "Unable to remove the budget: %s")}))
