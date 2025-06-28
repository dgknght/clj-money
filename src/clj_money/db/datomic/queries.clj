(ns clj-money.db.datomic.queries
  (:require [stowaway.datalog :as dtl]
            [clj-money.models.schema :as schema]))

(def ^:private default-opts
  {:relationships schema/relationships
   :query-prefix [:query]})

(defn apply-criteria
  [query criteria & {:as opts}]
  (dtl/apply-criteria query
                      criteria
                      (merge default-opts opts)))
