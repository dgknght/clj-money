(ns clj-money.authorization.receipt-ingestions
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:receipt-ingestion ::authorization/manage]
  [ingestion action user]
  (owner-or-granted? ingestion user action))

(defmethod authorization/scope :receipt-ingestion
  [_ user]
  (util/entity-type {:entity/user user}
                    :receipt-ingestion))
