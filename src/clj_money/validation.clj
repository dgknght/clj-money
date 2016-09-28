(ns clj-money.validation
  (:require [clojure.pprint :refer [pprint]]
            [schema.core :as schema]
            [schema.coerce :as coerce]
            [schema.utils :as schema-utils]
            [clj-money.inflection :refer [humanize]]
            [clj-money.schema :refer [friendly-message]]))

(defn validate-model
  "Validates the specified model using the specified rules.
  If any violations are found, they are added to the model
  in a sequence under the key :clj-money.validation/errors.
  
  A rule is simply a function that accepts the model and
  returns a sequence of tuples, one for each violation.
  Each tuple contains the key to whcih the violation applies
  in the first position and a user-friendly validation message
  in the second position"
  [model rules]
  (let [errors (mapcat #(% model) rules)]
    (when (seq errors)
      (assoc model ::errors (->> errors
                                 (group-by first)
                                 (map (fn [[k tuples]]
                                        [k (map second tuples)]))
                                 (into {}))))))

;; Schema validation
(defn- int-matcher
  [schema]
  (when (= schema/Int schema)
    (coerce/safe
      (fn [value]
        (if (and (string? value) (re-matches #"\d+" value))
          (Integer. value)
          value)))))

(defn- nil-matcher
  [schema]
  (when (= schema/Str schema)
    (coerce/safe
      (fn [value]
        (if (and (string? value) (= 0 (count value)))
          nil
          value)))))

(defn apply-schema
  "A rule function that coerces and applies a schema to a model"
  [schema model]
  (let [coercer (coerce/coercer schema
                                (coerce/first-matcher [int-matcher
                                                       nil-matcher
                                                       coerce/json-coercion-matcher]))
        result (coercer model)]
    (when (schema-utils/error? result)
      (map (fn [[k v]]
             [k (str (humanize k) " " (friendly-message v))])
           (schema-utils/error-val result)))))

(defn has-error?
  "Returns true if the specified model contains validation errors"
  [model]
  (contains? model ::errors))

(defn get-errors
  "Returns the errors from the specified model. If given only a model, 
  returns a map of all errors. If given a model and a key, returns the 
  errors for the specified key from wihin the model."
  ([model]
   (get model ::errors))
  ([model attr-key]
   (get-in model [::errors attr-key])))
