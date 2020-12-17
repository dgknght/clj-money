(ns clj-money.api.trading
  (:require [clj-money.api :as api]
            [clj-money.util :refer [serialize-date]]))

(defn create
  [{:keys [entity-id] :as trade} success-fn error-fn]
  {:pre [(:entity-id trade)]}

  (api/create-resource (api/path :entities entity-id :trades)
                       (-> trade
                           (dissoc :entity-id)
                           (update-in [:trade-date] serialize-date))
                       success-fn
                       error-fn))
