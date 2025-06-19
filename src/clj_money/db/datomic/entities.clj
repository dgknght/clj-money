(ns clj-money.db.datomic.entities
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as datomic]))

(def ^:private utc (t/zone-id "UTC"))

(defn- ->local-date
  [inst]
  (t/local-date inst utc))

(defmethod datomic/after-read :entity
  [entity]
  (-> entity
      (update-in-if [:entity/price-date-range 0] ->local-date)
      (update-in-if [:entity/price-date-range 1] ->local-date)
      (update-in-if [:entity/transaction-date-range 0] ->local-date)
      (update-in-if [:entity/transaction-date-range 1] ->local-date)
      (update-in-if [:entity/settings :settings/monitored-accounts] set)))
