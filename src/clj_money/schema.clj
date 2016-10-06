(ns clj-money.schema
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string])
  (:use clj-money.inflection))

(def rules
  [{:fn #(= % 'missing-required-key)
    :message "is required"}
   {:fn #(= % :duplicate-key)
    :message "is already in use"}
   {:fn #(re-find #"integer\?" (print-str %))
    :message "must be a number"}
   {:fn #(re-find #"invalid format" (print-str %))
    :message "is not valid"}
   {:fn (fn [violation]
          (when-let [match (re-find #"#\{((\S+\s?)*)\}" (print-str violation))]
            (-> match
                second
                (string/replace #"(?<=^| ):" "")
                (string/replace " " ", "))))
    :message "must be one of: %s"}
   {:fn #(re-find #"instance\? java\.lang\.String nil" (print-str %))
    :message "is required"}
   {:fn #(re-find #"not \(sequential" (print-str %))
    :message "must be sequential"}])

(defn friendly-message
  "Takes a single prismatic rule violation token and returns
  a user-friendly message"
  [violation]
  (if (vector? violation)
    (vec (map (fn [violation-map]
                (when violation-map
                  (->> violation-map
                       (map #(update-in % [1] friendly-message))
                       (into {})))) violation))
    (or (some (fn [{f :fn m :message}]
                (when-let [result (f violation)]
                  (if (seq? result)
                    (apply format m result)
                    (format m result))))
              rules)
        (print-str violation))))

(defn- extract-error
  "Extracts the error data from the exception ex-data"
  [validation-data]
  (if-let [error (:error validation-data)]
    error
    validation-data))

(defn user-friendify
  "Accepts the ex-data from a prismatic schema validation error and
  replaces esoteric validation rules with user-friendly messages"
  [validation-data]
  (->> validation-data
       extract-error
       (map #(update-in % [1] friendly-message))
       (into {})))

(defn- full-messages
  "Takes a map of user-friendly validation messages
  and creates full sentence validation messages"
  [errors]
  (->> errors
       (map (fn [[k m]]
              [k (str (humanize k) " " m)]))
       (into {})))

(defn append-errors
  "Appends error information from prismatic schema
  to the specified model"
  [model error-data]
  (assoc model :errors (-> error-data
                           user-friendify
                           full-messages)))
