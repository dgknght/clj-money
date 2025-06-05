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
(defn- resolve-ref
  [entry config]
  (update-in entry [1] #(if-let [v (config (naked-key %))]
                          v
                          (throw (ex-info (format "Unresolvable config reference: %s"
                                                  %)
                                          {:entry entry})))))

(defn process
  [config]
  (postwalk (fn [x]
              (if (config-ref? x)
                (resolve-ref x config)
                x))
            config))

(def env (process cfg/env))
