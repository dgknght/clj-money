(ns clj-money.api.transfers
  (:require [dgknght.app-lib.api :as api]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.authorization :refer [authorize] :as authorization]
            [clj-money.authorization.transfers]
            [clj-money.trading :as trading]
            [clj-money.dates :as dates]))

(def ^:private create-attributes
  [:transfer/date
   :transfer/shares
   :transfer/commodity
   :transfer/from-account
   :transfer/to-account])

(defn- extract-transfer
  [{:keys [params]}]
  (-> params
      (select-keys create-attributes)
      (update-in-if [:transfer/shares] bigdec)
      (update-in-if [:transfer/date] dates/ensure-local-date)))

(defn create
  [{:keys [authenticated] :as req}]
  (-> (extract-transfer req)
      (authorize ::authorization/create authenticated)
      trading/transfer
      (select-keys [:transfer/transaction
                    :transfer/lots])
      (update-in [:transfer/lots] (partial mapv #(-> %
                                                     (update-in [:lot/account] util/->entity-ref)
                                                     (update-in [:lot/commodity] util/->entity-ref))))
      api/creation-response))

(def routes
  ["entities/:entity-id/transfers" {:post {:handler create}}])
