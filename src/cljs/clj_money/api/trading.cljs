(ns clj-money.api.trading
  (:require [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api :as api]))

(defn create
  [{:keys [entity-id] :as trade} success-fn error-fn]
  {:pre [(:entity-id trade)]}

  (api/post (api/path :entities entity-id :trades)
            (-> trade
                (dissoc :entity-id)
                (update-in [:trade-date] serialize-date))
            success-fn
            error-fn))
