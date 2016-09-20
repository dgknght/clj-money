(ns clj-money.schema
  (:require [clojure.pprint :refer [pprint]])
  (:import schema.utils.ValidationError))

(def rules
  [{:fn #(= % 'missing-required-key)
    :message "is required"}
   {:fn #(re-find #"integer\?" (print-str %))
    :message "must be a number"}
   {:fn #(re-find #"invalid format" (print-str %))
    :message "is not valid"}])

(defn- friendly-message
  "Takes a single prismatic rule violation token and returns
  a user-friendly message"
  [violation]
  (some (fn [{f :fn m :message}]
          (when (f violation)
            m))
        rules))

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
