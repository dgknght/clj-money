(ns clj-money.db.datomic.users
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as dat]))

(defmethod dat/before-save :user
  [user]
  (update-in-if user [:user/token-expires-at] t/java-date))
