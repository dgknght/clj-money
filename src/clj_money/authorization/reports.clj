(ns clj-money.authorization.reports
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

; A report isn't stored, so it's checked as a pseudo-resource,
; {:report/entity entity}, the entity it's run on
(defmethod authorization/allowed? [:report ::authorization/manage]
  [report action user]
  (owner-or-granted? report user action))
