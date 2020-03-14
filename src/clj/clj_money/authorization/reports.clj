(ns clj-money.authorization.reports
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/report ::authorization/manage]
  [report action user]
  (owner-or-granted? report user action))
