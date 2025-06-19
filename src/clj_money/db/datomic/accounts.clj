(ns clj-money.db.datomic.accounts
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :account
  [account]
  (-> account
      (update-in-if [:account/system-tags] set)
      (update-in-if [:account/user-tags] set)))
