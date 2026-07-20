(ns clj-money.db.sql.queries
  (:require [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk]]
            [stowaway.sql-qualified :as sql]
            [clj-money.entities.schema :as schema]
            [clj-money.util :as util]))

(def ^:private joins
  (merge
    (->> (schema/build :sql)
         (filter (fn [[_ e]]
                   (some map? (:refs e))))
         (mapcat (fn [[id {:keys [refs]}]]
                   (->> refs
                        (filter (every-pred map?
                                            :columns))
                        (map (fn [{:keys [columns] :as ref}]
                               [[(:id ref) id]
                                columns])))))
         (into {}))
    ; Explicit JOIN for array-based relationships
    {[:lot :lot-note] [[:id [:any :lot-note/lot-ids]]]}))

(def ^:private relationships
  (->> (schema/build :sql)
       (mapcat (fn [[id {:keys [refs]}]]
                 (map #(vector (schema/relationship-ref-type %) id)
                      refs)))
       set))

(def ^:private default-options
  {:relationships relationships
   :joins joins})

(def ^:private bisect
  (juxt namespace name))

(defn- self->id
  [m]
  (postwalk (fn [x]
              (if (map-entry? x)
                (let [k (key x)
                      [n a] (bisect k)]
                  (if (= "_self" a)
                    [(keyword n "id") (:id (val x))]
                    x))
                x))
            m))

(defn criteria->query
  [criteria & [options]]
  {:pre [criteria
         (or (:entity-type options)
             (util/entity-type criteria))]}
  (-> criteria
      (self->id)
      (sql/->query (merge default-options options))))

(defn ->update
  [changes criteria & [options]]
  (sql/->update changes criteria (merge default-options options)))
