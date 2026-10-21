(ns clj-money.authorization.statement-ingestions
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:statement-ingestion ::authorization/manage]
  [ingestion action user]
  (owner-or-granted? ingestion user action))

(defmethod authorization/scope :statement-ingestion
  [_ user action]
  (entity-scope :statement-ingestion :statement-ingestion/entity user action))
