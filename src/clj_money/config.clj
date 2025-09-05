(ns clj-money.config
  (:require [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            [config.core :as cfg]))

(defn- config-ref?
  [x]
  (and (map-entry? x)
       (let [v (val x)]
         (and (keyword? v)
              (= "config" (namespace v))))))

(def ^:private naked-key (comp keyword name))

(defn- extract-value
  [config]
  (fn [ky]
    (let [k (naked-key ky)]
      (when-not (contains? config k)
        (throw (ex-info (format "Unresolvable config reference: %s"
                              k)
                      {:config config})))
      (get-in config [k]))))

(defn- resolve-ref
  [entry config]
  (update-in entry [1] (extract-value config)))

(defn process
  [config]
  (postwalk (fn [x]
              (if (config-ref? x)
                (resolve-ref x config)
                x))
            config))

(def env (process cfg/env))
