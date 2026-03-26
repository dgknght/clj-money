(ns clj-money.db.sql.invitations
  (:require [clj-money.db.sql :as sql]))

(defmethod sql/after-read :invitation
  [invitation]
  (update-in invitation [:invitation/status] keyword))
