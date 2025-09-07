(ns clj-money.monitoring
  (:require [clojure.tools.logging :as log])
  (:import [java.lang Runtime]))

(defn log-memory-usage
  [message]
  (let [runtime (Runtime/getRuntime)
        free (.freeMemory runtime)]
    (log/debugf "[memory] %s. Used memory: %,d. Free memory: %,d"
                message
                (- (.totalMemory runtime)
                   free)
                free)))
