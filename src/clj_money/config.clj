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

(defn- resolve-ref
  [entry config]
  (update-in entry [1] #(if-let [v (config %)]
                          v
                          (throw (ex-info "Unresolvable config reference" {:entry entry})))))

(defn process
  [config]
  (postwalk (fn [x]
              (if (config-ref? x)
                (resolve-ref x config)
                x))
            config))

(def env (process cfg/env))
