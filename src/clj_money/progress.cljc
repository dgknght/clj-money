(ns clj-money.progress
  (:refer-clojure :exclude [get])
  #?(:clj (:require [clj-money.config :refer [env]])))

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

(defmulti reify-tracker
  (fn [config & _]
    (::strategy config)))

(defn- config []
  #?(:clj (let [active-key (get-in env [:progress :active])]
            (get-in env [:progress :strategies active-key]))
     :cljs (throw (js/Error. "Not implemented"))))

(defn tracker
  [& args]
  (apply reify-tracker (config) args))
