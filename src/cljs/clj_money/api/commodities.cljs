(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count get])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [unserialize-date]]
            [clj-money.state :refer [current-entity]]
            [dgknght.app-lib.api :as api]))

(defn count
  [success-fn error-fn]
  (api/get (api/path :entities (:id @current-entity) :commodities :count)
           success-fn
           error-fn))

(defn- after-read
  [commodity]
  (update-in-if commodity [:most-recent-price :trade-date] unserialize-date))

(defn select
  ([success-fn error-fn]
   (select {} success-fn error-fn))
  ([criteria success-fn error-fn]
   (api/get (api/path :entities (:id @current-entity) :commodities)
            criteria
            (comp success-fn
                  #(map after-read %))
            error-fn)))

(defn get
  [id success-fn error-fn]
  (api/get (api/path :commodities id) success-fn error-fn))

(defn create
  [commodity success-fn error-fn]
  (api/post (api/path :entities (:entity-id commodity) :commodities)
            commodity
            success-fn
            error-fn))

(defn update
  [commodity success-fn error-fn]
  (api/patch (api/path :commodities (:id commodity))
             commodity
             success-fn
             error-fn))

(defn save
  [commodity success-fn error-fn]
  (if (:id commodity)
    (update commodity success-fn error-fn)
    (create commodity success-fn error-fn)))

(defn delete
  [commodity success-fn error-fn]
  (api/delete (api/path :commodities (:id commodity))
              success-fn
              error-fn))
