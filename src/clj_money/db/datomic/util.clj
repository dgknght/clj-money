(ns clj-money.db.datomic.util
  (:require [clj-money.util :as util]))

(defn- extract-model-id
  ([x]
   (if (map-entry? x)
     (update-in x [1] extract-model-id)
     (if (util/model-ref? x)
       (:id x)
       x))))

(defn- ->datom
  [id]
  (fn [[k v]]
    (if v
      [:db/add id k v]
      [:db/retract id k])))

(defn ->datoms
  [m]
  {:pre [(map? m)]}

  (let [id (or (:id m)
               (util/temp-id))]
    (->> m
         (remove #(= :id (key %)))
         (map (comp (->datom id)
                    extract-model-id)))))
