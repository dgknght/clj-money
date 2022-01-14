(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count get])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [unserialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :refer [handle-ex]]))

(defn count
  [xf]
  (api/get (api/path :entities (:id @current-entity) :commodities :count)
           {:transform xf
            :handle-ex (handle-ex "Unable to get a count of commodities: %s")}))

(defn- after-read
  [commodity]
  (-> commodity
      (update-in-if [:earliest-price] unserialize-date)
      (update-in-if [:latest-price] unserialize-date)
      (update-in-if [:most-recent-price :trade-date] unserialize-date)))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read) xf))

(defn select
  ([xf]
   (select {} xf))
  ([criteria xf]
   (api/get (api/path :entities (:id @current-entity) :commodities)
            criteria
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to retrieve the commodities: %s")})))

(defn get
  [id xf]
  (api/get (api/path :commodities id)
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the commdoity: %s")}))

(defn create
  [commodity xf]
  (api/post (api/path :entities (:entity-id commodity) :commodities)
            commodity
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to create the commodity: %s)")}))

(defn update
  [commodity xf]
  (api/patch (api/path :commodities (:id commodity))
             commodity
             {:transform (transform xf)
              :handle-ex (handle-ex "Unable to update the commodity: %s")}))

(defn save
  [commodity xf]
  (let [f (if (:id commodity)
            update
            create)]
    (f commodity xf)))

(defn delete
  [commodity xf]
  (api/delete (api/path :commodities (:id commodity))
              {:transform xf
               :handle-ex (handle-ex "Unable to remove the commodity: %s")}))
