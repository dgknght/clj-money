(ns clj-money.db.datomic.attachments
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as datomic]
            [clj-money.dates :refer [->local-date]]))

(defmethod datomic/before-save :attachment
  [att]
  (dissoc att :attachment/transaction-date))

(defmethod datomic/after-read :attachment
  [att]
  (update-in-if att [:attachment/transaction-date] ->local-date))
