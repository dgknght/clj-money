(ns clj-money.authorization.trades
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:trade ::authorization/manage]
  [trade action user]
  (owner-or-granted? trade user action))
