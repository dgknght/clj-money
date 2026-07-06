(ns clj-money.validation
  (:require [clojure.string :as string]))

(defn- validation-msgs
  [m]
  (->> (vals m)
       (filter sequential?)
       (mapcat str)
       (string/join "; ")))

(defn readable
  "Given the exception thrown for a non-2xx API response, if the ex-data
  is a map of validation errors (attribute path -> vector of message
  strings), join the messages into a single string. Returns nil for any
  other ex-data shape (e.g. {:message \"forbidden\"})."
  [e]
  (when-let [data (ex-data e)]
    (seq (validation-msgs data)))
  #_(let [data (ex-data e)]
    (when (and (map? data)
               (seq data)
               (every? #(and (sequential? %)
                             (every? string? %))
                       (vals data)))
      (->> (vals data)
           (mapcat identity)
           (string/join "; ")))))
