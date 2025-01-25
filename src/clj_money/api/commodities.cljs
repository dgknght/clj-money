(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count get])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.models :as models]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [handle-ex]]))

(defn count
  [criteria & {:as opts}]
  (api/get (api/path :entities @current-entity :commodities :count)
           criteria
           (merge
             {:on-error (handle-ex "Unable to get a count of commodities: %s")}
             opts)))

(defn select
  [criteria & {:as opts}]
  (api/get (api/path :entities @current-entity :commodities)
           criteria
           (merge
               {:on-error (handle-ex "Unable to retrieve the commodities: %s")}
               opts)))

(defn create
  [commodity opts]
  (api/post (api/path :entities @current-entity :commodities)
            commodity
            (merge
               {:on-error (handle-ex "Unable to create the commodity: %s")}
               opts)))

(defn update
  [commodity opts]
  (api/patch (api/path :commodities commodity)
             commodity
             (merge
               {:on-error (handle-ex "Unable to update the commodity: %s")}
               opts)))

(defn save
  [commodity & {:as opts}]
  (let [f (if (:id commodity)
            update
            create)]
    (-> commodity
        (models/prune :commodity
                      :exclude [:commodity/entity])
        (f opts))))

(defn delete
  [commodity & {:as opts}]
  (api/delete (api/path :commodities commodity)
              (merge
               {:on-error (handle-ex "Unable to delete the commodity: %s")}
               opts)))
