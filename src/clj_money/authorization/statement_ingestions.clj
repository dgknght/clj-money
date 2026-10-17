(ns clj-money.authorization.statement-ingestions
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:statement-ingestion ::authorization/manage]
  [ingestion action user]
  (owner-or-granted? ingestion user action))

(defmethod authorization/scope :statement-ingestion
  [_ user]
  (util/entity-type {:entity/user user}
                    :statement-ingestion))
