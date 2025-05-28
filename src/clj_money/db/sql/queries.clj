(ns clj-money.db.sql.queries
  (:refer-clojure :exclude [format])
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.sql-qualified :as sql]
            [clj-money.models.schema :as schema]
            [clj-money.util :as util]))

(def ^:private relationships
  (->> schema/models
       (mapcat (fn [{:keys [id refs]}]
                 (map #(vector (schema/ref-id %) id)
                      refs)))
       set))

(def ^:private joins
  (->> schema/models
       (filter #(some map? (:refs %)))
       (mapcat (fn [{:keys [refs id]}]
                 (->> refs
                      (filter map?)
                      (map (fn [{:keys [columns] :as ref}]
                             [[(:id ref) id]
                              columns])))))
       (into {})))

(def ^:private default-options
  {:relationships relationships
   :joins joins})

(defn criteria->query
  [criteria & [options]]
  {:pre [criteria
         (util/model-type criteria)]}
  (sql/->query criteria (merge default-options options)))

(defn ->update
  [changes criteria & [options]]
  (sql/->update changes criteria (merge default-options options)))
