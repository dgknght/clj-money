(ns clj-money.api.trading
  (:require [clj-money.api :as api]
            [clj-money.x-platform.util :refer [serialize-date]]))

(defn create
  [{:keys [entity-id] :as trade} success-fn error-fn]
  (api/create-resource (api/path :entities entity-id :trades)
                       (-> trade
                           (dissoc :entity-id)
                           (update-in [:trade-date] serialize-date))
                       success-fn
                       error-fn))
