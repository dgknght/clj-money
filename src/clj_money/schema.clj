(ns clj-money.schema
  (:require [clojure.pprint :refer [pprint]])
  (:import schema.utils.ValidationError))

(def validation-error-map
  {#"integer\?"      "must be a number"
   #"invalid format" "is not valid"})

(defn- translate-validation-error
  "Takes a ValidationError instance and returns a user-friendly
  message"
  [error]
  (some (fn [[pattern message]]
          (when (re-find pattern (print-str error))
            message)) 
        validation-error-map))

(defn- friendly-message
  "Takes a single prismatic rule violation token and returns
  a user-friendly message"
  [violation]
  (cond
    (= violation 'missing-required-key) "is required"
    (instance? ValidationError violation) (translate-validation-error violation)
    :else violation))

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
