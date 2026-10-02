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
                      {:keys (-> config keys sort)})))
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

(defn protect-file-config
  "Returns the merged configuration with the values from the config file
  restored over any environment variables that replaced them, if the
  config file sets :env-var-overrides? to false. JVM system properties still
  override everything. This keeps a shell that exports the development
  settings from pointing the tests at the development database."
  [merged file-config system-props]
  (if (false? (:env-var-overrides? file-config))
    (cfg/merge-maps merged file-config system-props)
    merged))

(def env (process (protect-file-config cfg/env
                                       (cfg/read-config-file "config.edn")
                                       (cfg/read-system-props))))
