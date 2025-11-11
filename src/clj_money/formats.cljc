(ns clj-money.formats
  (:require [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            [camel-snake-kebab.core :refer [->camelCase
                                            ->kebab-case]]))

(def ^:private strip-ns
  (comp keyword name))

(defn- dominant-ns
  [m]
  (->> (keys m)
       (map namespace)
       frequencies
       (sort-by second >)
       (map (comp keyword first))
       first))

(defn- edn-map->json
  [{:as m :keys [id]}]
  (let [type (dominant-ns m)]
    (cond-> (-> m
                (dissoc :id)
                (update-keys (comp ->camelCase
                                   strip-ns)))
      type (assoc :_type type)
      id (assoc :id id))))

(defn edn->json
  [input]
  (postwalk (fn [x]
              (if (map? x)
                (edn-map->json x)
                x))
            input))

(defn- +ns
  [n]
  (let [n-str (name n)]
    (fn [k]
      (keyword n-str (name k)))))

(defn- json-map->edn
  [{:as m :keys [id]}]
  (if-let [type (:_type m)]
    (cond-> (-> m
                (dissoc :_type :id)
                (update-keys (comp (+ns type)
                                   ->kebab-case)))
      id (assoc :id id))
    m))

(defn json->edn
  [input]
  (postwalk (fn [x]
              (if (map? x)
                (json-map->edn x)
                x))
            input))
