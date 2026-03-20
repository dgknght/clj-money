(ns clj-money.db.datomic.lot-notes
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :lot-note
  [note]
  (update-in-if note [:lot-note/transaction-date] ->local-date))
