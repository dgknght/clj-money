(ns clj-money.db.sql.queries
  (:refer-clojure :exclude [format])
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.sql-qualified :as sql]
            [clj-money.models.schema :as schema]
            [clj-money.util :as util]))

(def ^:private relationships
  (->> schema/models
       (mapcat (fn [model]
                 (map #(vector % (:type model))
                      (:refs model))))
       set))

(def ^:private default-options
  {:relationships relationships})

(defn criteria->query
  [criteria & [options]]
  {:pre [criteria
         (util/model-type criteria)]}
  (sql/->query criteria (merge default-options options)))

(defn ->update
  [changes criteria & [options]]
  (sql/->update changes criteria (merge default-options options)))
