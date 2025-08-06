(ns clj-money.api.commodities
  (:refer-clojure :exclude [update count get])
  (:require [clj-money.state :refer [current-entity]]
            [clj-money.util :as util]
            [clj-money.models.schema :as schema]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn count
  [& {:as opts}]
  (api/get (api/path :entities @current-entity :commodities :count)
           (add-error-handler
             opts
             "Unable to get a count of commodities: %s")))

(defn select
  [criteria & {:as opts}]
  (api/get (api/path :entities @current-entity :commodities)
           criteria
           (add-error-handler
             opts
             "Unable to retrieve the commodities: %s")))

(defn create
  [{:as commodity :commodity/keys [entity]} opts]
  (api/post (api/path :entities entity :commodities)
            commodity
            (add-error-handler
              opts
               "Unable to create the commodity: %s)")))

(defn update
  [commodity opts]
  (api/patch (api/path :commodities commodity)
             commodity
             (add-error-handler
               opts
               "Unable to update the commodity: %s")))

(defn save
  [commodity & {:as opts}]
  (let [f (if (:id commodity)
            update
            create)]
    (-> commodity
        (schema/strip :commodity)
        (util/pp-> ::save)
        (f opts))))

(defn delete
  [commodity & {:as opts}]
  (api/delete (api/path :commodities commodity)
              (add-error-handler
                opts
                "Unable to remove the commodity: %s")))
