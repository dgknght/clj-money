(ns clj-money.authorization.receipt-ingestions
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?
                                                     entity-scope]]))

(defmethod authorization/allowed? [:receipt-ingestion ::authorization/manage]
  [ingestion action user]
  (owner-or-granted? ingestion user action))

(defmethod authorization/scope :receipt-ingestion
  [_ user action]
  (entity-scope :receipt-ingestion :receipt-ingestion/entity user action))
