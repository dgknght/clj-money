(ns clj-money.api.trading
  (:require [clj-money.models :refer [prune]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  [{:trade/keys [entity] :as trade} & {:as opts}]
  {:pre [(:trade/entity trade)]}

  (api/post (api/path :entities entity :trades)
            (-> trade
                (prune :trade)
                (dissoc :trade/entity))
            (add-error-handler opts "Unable to create the trading transaction: %s")))
