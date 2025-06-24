(ns clj-money.db.datomic.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.util :refer [+id temp-id]]
            [clj-money.db.datomic :as datomic]))

(def ^:private utc (t/zone-id "UTC"))

; TODO: Dedupe this with the entities ns
(defn- ->local-date
  [inst]
  (t/local-date inst utc))

(defmethod datomic/before-save :transaction
  [trx]
  (update-in trx
             [:transaction/items]
             (fn [items]
               (mapv #(+id % temp-id) items))))

(defmethod datomic/after-read :transaction
  [trx]
  (update-in trx [:transaction/transaction-date] ->local-date))
