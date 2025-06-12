(ns clj-money.api
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]))

(defn log-error
  [error message]
  (log/errorf "%s: %s - %s\n  %s"
              message
              (.getClass error)
              (.getMessage error)
              (->> (.getStackTrace error)
                   (map str)
                   (string/join "\n  "))))
