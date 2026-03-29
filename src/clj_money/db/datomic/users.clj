(ns clj-money.db.datomic.users
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :user
  [user]
  (dissoc user :user/password-confirmation))

(defmethod datomic/after-read :user
  [user]
  (update-in-if user [:user/roles] set))
