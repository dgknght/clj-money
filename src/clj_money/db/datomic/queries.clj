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

(defn apply-select
  [query & {:keys [select select-also] :as opts}]
  (cond-> query
    select (dtl/apply-select select (merge default-opts
                                           opts
                                           {:replace true}))
    select-also (dtl/apply-select select-also (merge default-opts opts))))
