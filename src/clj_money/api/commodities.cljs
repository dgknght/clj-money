(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count get])
  (:require [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [handle-ex]]))

(defn count
  [xf]
  (api/get (api/path :entities (:id @current-entity) :commodities :count)
           {:transform xf
            :handle-ex (handle-ex "Unable to get a count of commodities: %s")}))

(defn select
  ([xf]
   (select {} xf))
  ([criteria xf]
   (api/get (api/path :entities (:id @current-entity) :commodities)
            criteria
            {:transform xf
             :handle-ex (handle-ex "Unable to retrieve the commodities: %s")})))

(defn create
  [commodity xf]
  (api/post (api/path :entities (:commodity/entity commodity) :commodities)
            commodity
            {:transform xf
             :handle-ex (handle-ex "Unable to create the commodity: %s)")}))

(defn update
  [commodity xf]
  (api/patch (api/path :commodities commodity)
             commodity
             {:transform xf
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
