(ns clj-money.api.trading
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  [{:trade/keys [entity] :as trade} & {:as opts}]
  {:pre [(:trade/entity trade)]}

  (api/post (api/path :entities entity :trades)
            (-> trade
                (select-keys [:trade/date
                              :trade/action
                              :trade/value
                              :trade/dividend?
                              :trade/account
                              :trade/commodity
                              :trade/dividend-account])
                (update-in-if [:trade/account] util/->model-ref)
                (update-in-if [:trade/commodity] util/->model-ref)
                (update-in-if [:trade/dividend-account] util/->model-ref)
                (dissoc :trade/entity))
            (add-error-handler opts "Unable to create the trading transaction: %s")))
