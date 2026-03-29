(ns clj-money.authorization.invitations
  (:require [clj-money.authorization :as authorization]))

(defmethod authorization/allowed? [:invitation ::authorization/manage]
  [_ _ user]
  (contains? (:user/roles user) :admin))

(defmethod authorization/scope :invitation
  [_ _]
  {})
