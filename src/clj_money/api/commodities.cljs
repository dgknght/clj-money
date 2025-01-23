(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count get])
  (:require [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [handle-ex]]))

(defn count
  [xf]
  (api/get (api/path :entities (:id @current-entity) :commodities :count)
           {:post-xf xf
            :handle-ex (handle-ex "Unable to get a count of commodities: %s")}))

(defn select
  [criteria & {:as opts}]
  (api/get (api/path :entities (:id @current-entity) :commodities)
           criteria
           (assoc opts :on-error (handle-ex "Unable to retrieve the commodities: %s"))))

(defn create
  [commodity opts]
  (api/post (api/path :entities (:commodity/entity commodity) :commodities)
            commodity
            (assoc opts :on-error (handle-ex "Unable to create the commodity: %s"))))

(defn update
  [commodity opts]
  (api/patch (api/path :commodities commodity)
             commodity
             (assoc opts :on-error (handle-ex "Unable to update the commodity: %s"))))

(defn save
  [commodity & {:as opts}]
  (let [f (if (:id commodity)
            update
            create)]
    (f commodity opts)))

(defn delete
  [commodity xf]
  (api/delete (api/path :commodities (:id commodity))
              {:post-xf xf
               :handle-ex (handle-ex "Unable to remove the commodity: %s")}))
