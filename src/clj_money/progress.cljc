(ns clj-money.progress
  (:refer-clojure :exclude [get])
  #?(:clj (:require [integrant.core :as ig]
                    [clj-money.config :refer [env]])))

(defprotocol Tracker
  "Functions that track progress of a multi-part, long running process"
  (expect [this process-key expected-count]
          "Indicates that there is a process that is expecting the specified number of iterations")
  (increment [this process-key]
             [this process-key completed-count]
             "Indicates that some number (default 1) of expected iterations have completed.")
  (get [this]
       "Returns a map of all the specified process keys and their progress

       A return value has this shape:
       {:processes {:process-1 {:total 100 :completed 50}
                    :process-2 {:total 20 :completed 20}}
        :warnings [\"warning 1\"]
        :failure-reason \"this bad thing happended\"
        :finished true}")
  (warn [this msg]
        "Record a notification about the process")
  (fail [this msg]
        "Record a notification about the process")
  (finish [this]
          "Indicate that the process has finished"))

(defprotocol TrackerFactory
  "Creates trackers, holding any resources (e.g., connection pools)
  shared by the trackers it creates"
  (create-tracker [this root-key] "Returns a new tracker for the given root key")
  (close [this] "Releases any resources held by the factory"))

(defmulti reify-tracker
  (fn [config & _]
    (::strategy config)))

(defmulti reify-tracker-factory ::strategy)

(defmethod reify-tracker-factory :default
  [config]
  (reify TrackerFactory
    (create-tracker [_ root-key]
      (reify-tracker config root-key))
    (close [_] nil)))

(def ^:dynamic *tracker-factory* nil)

#?(:clj
   (do
     (defn active-config
       "Returns the config for the active progress tracking strategy"
       [env]
       (get-in env [:progress :strategies (get-in env [:progress :active])]))

     (defmethod ig/init-key ::tracker-factory
       [_ config]
       (reify-tracker-factory config))

     (defmethod ig/halt-key! ::tracker-factory
       [_ factory]
       (close factory))

     ; Until every entry point binds *tracker-factory* from the Integrant
     ; system, fall back to a single factory shared by the whole process.
     (def ^:private default-tracker-factory
       (delay (reify-tracker-factory (active-config env))))))

(defn tracker-factory []
  (or *tracker-factory*
      #?(:clj @default-tracker-factory
         :cljs (throw (js/Error. "Not implemented")))))

(defn tracker
  [root-key]
  (create-tracker (tracker-factory) root-key))
