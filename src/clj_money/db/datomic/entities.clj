(ns clj-money.db.datomic.entities
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :entity
  [entity]
  (-> entity
      (update-in-if [:entity/price-date-range 0] ->local-date)
      (update-in-if [:entity/price-date-range 1] ->local-date)
      (update-in-if [:entity/transaction-date-range 0] ->local-date)
      (update-in-if [:entity/transaction-date-range 1] ->local-date)
      (update-in-if [:entity/settings :settings/monitored-accounts] set)))
