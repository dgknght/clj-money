(ns clj-money.db.sql.queries
  (:refer-clojure :exclude [format])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk]]
            [stowaway.sql-qualified :as sql]
            [clj-money.models.schema :as schema]
            [clj-money.util :as util]))

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
  {:relationships schema/relationships
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
         (util/model-type criteria)]}
  (-> criteria
      (self->id)
      (sql/->query (merge default-options options))))

(defn ->update
  [changes criteria & [options]]
  (sql/->update changes criteria (merge default-options options)))
