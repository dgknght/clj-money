(ns clj-money.api.trading
  (:require [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [handle-ex]]))

(defn create
  [{:keys [entity-id] :as trade} xf]
  {:pre [(:entity-id trade)]}

  (api/post (api/path :entities entity-id :trades)
            (-> trade
                (dissoc :entity-id)
                (update-in [:trade-date] serialize-date))
            {:transform xf
             :handle-ex (handle-ex "Unable to create the trading transaction: %s")}))
