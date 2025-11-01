(ns clj-money.formats
  (:require [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            [camel-snake-kebab.core :refer [->camelCase
                                            ->kebab-case]]
            [clj-money.util :as util]))

(def ^:private strip-ns
  (comp keyword name))

(defn- edn-map->json
  [m]
  (let [type (util/single-ns m
                             :ignore #{:id}
                             :allow-none true)]
    (cond-> (update-keys m
                         (comp ->camelCase
                               strip-ns))
      type (assoc :_type type))))

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
  [m]
  {:pre [(:_type m)]}
  (-> m
      (dissoc :_type)
      (update-keys (comp (+ns (:_type m))
                         ->kebab-case))))

(defn json->edn
  [input]
  (postwalk (fn [x]
              (if (and (map? x)
                       (not= #{:id} (->> x keys set)))
                (json-map->edn x)
                x))
            input))
