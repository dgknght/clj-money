(ns clj-money.api.trading
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- adjust-value-for-fee
  "When value-includes-fee? is true, normalize value to shares × price before
  sending to the API: subtract fee for purchases, add it back for sales."
  [{:trade/keys [action fee value-includes-fee? value] :or {fee 0} :as trade}]
  (if (and value-includes-fee? (pos? fee))
    (assoc trade :trade/value (if (= :sell action)
                                (+ value fee)
                                (- value fee)))
    trade))

(defn create
  [{:trade/keys [entity] :as trade} & {:as opts}]
  {:pre [(:trade/entity trade)]}

  (api/post (api/path :entities entity :trades)
            (-> trade
                adjust-value-for-fee
                (select-keys [:trade/date
                              :trade/action
                              :trade/value
                              :trade/shares
                              :trade/fee
                              :trade/fee-account
                              :trade/dividend?
                              :trade/account
                              :trade/commodity
                              :trade/dividend-account])
                (update-in-if [:trade/account] util/->entity-ref)
                (update-in-if [:trade/commodity] util/->entity-ref)
                (update-in-if [:trade/fee-account] util/->entity-ref)
                (update-in-if [:trade/dividend-account] util/->entity-ref)
                (dissoc :trade/entity))
            (add-error-handler opts "Unable to create the trading transaction: %s")))
