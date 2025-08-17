(ns clj-money.db.datomic.users
  (:require [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :user
  [user]
  (dissoc user :user/password-confirmation))
