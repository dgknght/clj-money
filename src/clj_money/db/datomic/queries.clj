(ns clj-money.db.datomic.queries
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [stowaway.datalog :as dtl]
            [clj-money.entities.schema :as schema]))

; TODO: reconcile this with the schema namespace
(def ^:private relationships
  (set/union
    (set/difference
      schema/relationships
      #{[:budget :budget-item]
        [:transaction :lot-item]
        [:transaction :transaction-item]})
    #{[:budget-item :budget :items]
      [:lot-item :transaction :lot-items]
      [:transaction-item :transaction :items]}))

(def ^:private default-opts
  {:relationships relationships
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
