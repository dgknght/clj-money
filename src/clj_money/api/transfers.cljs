(ns clj-money.api.transfers
  (:require [clj-money.util :as util]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  [transfer & {:as opts}]
  (api/post (api/path :entities @current-entity :transfers)
            (-> transfer
                (select-keys [:transfer/date
                              :transfer/shares
                              :transfer/commodity
                              :transfer/from-account
                              :transfer/to-account])
                (update-in [:transfer/commodity] util/->entity-ref)
                (update-in [:transfer/from-account] util/->entity-ref)
                (update-in [:transfer/to-account] util/->entity-ref))
            (add-error-handler opts "Unable to create the transfer: %s")))
