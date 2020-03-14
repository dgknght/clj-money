(ns clj-money.authorization.trades
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/trade ::authorization/manage]
  [trade action user]
  (owner-or-granted? trade user action))
