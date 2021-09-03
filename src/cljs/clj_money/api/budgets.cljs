(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [clojure.walk :refer [keywordize-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.decimal :refer [->decimal]]

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

(defn search
  [success-fn error-fn]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :budgets)
           #(success-fn (map after-read %))
           error-fn))

(defn find
  [id success-fn error-fn]
  (api/get (api/path :budgets id)
           (comp success-fn after-read)
           error-fn))

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
  [budget success-fn error-fn]
  (api/patch
    (api/path :budgets
              (:id budget))
    budget
    success-fn
    error-fn))

(defn- create
  [budget success-fn error-fn]
  (api/post
    (api/path :entities
              (:id @current-entity)
              :budgets)
    (update-in-if budget [:auto-create-start-date] serialize-date)
    success-fn
    error-fn))

(defn save
  [budget success-fn error-fn]
  (let [f (if (:id budget)
            update
            create)]
    (f (before-save budget)
       (comp success-fn
             after-read)
       error-fn)))

(defn delete
  [budget success-fn error-fn]
  (api/delete (api/path :budgets (:id budget))
              success-fn
              error-fn))
