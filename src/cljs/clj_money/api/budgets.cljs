(ns clj-money.api.budgets
  (:refer-clojure :exclude [update find])
  (:require [clj-money.api :as api]
            [clj-money.decimal :refer [->decimal]]
            [clj-money.util :refer [update-in-if
                                    serialize-date
                                    unserialize-date]]
            [clj-money.state :refer [current-entity]]))

(defn- after-item-read
  [item]
  (update-in item [:periods] #(mapv ->decimal %)))

(defn- after-read
  [budget]
  (-> budget
      (update-in-if [:items] #(map after-item-read %))
      (update-in [:period] keyword)
      (update-in [:start-date] unserialize-date)
      (update-in [:end-date] unserialize-date)))

(defn search
  [success-fn error-fn]
  (api/get-resources (api/path :entities
                               (:id @current-entity)
                               :budgets)
                     #(success-fn (map after-read %))
                     error-fn))

(defn find
  [id success-fn error-fn]
  (api/get-resource (api/path :budgets id)
                    (comp success-fn after-read)
                    error-fn))

(defn- before-save
  [budget]
  (update-in budget [:start-date] serialize-date))

(defn- update
  [budget success-fn error-fn]
  (api/update-resource
   (api/path :budgets
             (:id budget))
   budget
   success-fn
   error-fn))

(defn- create
  [budget success-fn error-fn]
  (api/create-resource
   (api/path :entities
             (:id @current-entity)
             :budgets)
   budget
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
  (api/delete-resource (api/path :budgets (:id budget))
                       success-fn
                       error-fn))
