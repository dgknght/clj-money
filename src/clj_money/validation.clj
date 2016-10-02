(ns clj-money.validation
  (:require [clojure.pprint :refer [pprint]]
            [schema.core :as schema]
            [schema.coerce :as coerce]
            [schema.utils :as schema-utils]
            [clj-money.inflection :refer [humanize]]
            [clj-money.schema :refer [friendly-message]]))

(defn- apply-rule
  "Applies the rule to the context, returning an 
  updated context. The context contains
  {:model  the model being validated
  :errors the list of validation errors}"
  [context rule]
  (let [{validated :model
         errors :errors} (rule (:model context))]
    (-> context
        (update-in [:errors] #(concat  % errors))
        (assoc :model validated))))

(defn validate-model
  "Validates the specified model using the specified rules.
  If any violations are found, they are added to the model
  in a sequence under the key :clj-money.validation/errors.
  
  A rule is simply a function that accepts the model and
  returns a map containing the following

  {:model  The model (in case it was updated by the rule)
   :errors A sequence of tuples containing the key in the 1st postion
           and a rule violation in the 2nd}"
  [model rules]
  (let [{validated :model
         errors :errors} (reduce apply-rule
                                 {:errors []
                                  :model model}
                                 rules)]
    (if (seq errors)
      (assoc validated ::errors (->> errors
                                     (group-by first)
                                     (map (fn [[k tuples]]
                                            [k (map second tuples)]))
                                     (into {})))
      validated)))

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

(defn- full-humanized-message
  "Accepts a tuple containg an attribute key and a schema
  violation expresion and returns a human-friendly message"
  [[attr-key expr]]
  [attr-key (str (humanize attr-key) " " (friendly-message expr))])

(defn apply-schema
  "A rule function that coerces and applies a schema to a model"
  [schema model]
  (let [coercer (coerce/coercer schema
                                (coerce/first-matcher [int-matcher
                                                       nil-matcher
                                                       coerce/json-coercion-matcher]))
        result (coercer model)]
    {:model (dissoc result :error)
     :errors (if (schema-utils/error? result)
               (map full-humanized-message
                    (schema-utils/error-val result))
               [])}))

(defn has-error?
  "Returns true if the specified model contains validation errors"
  ([model]
   (contains? model ::errors))
  ([model attribute]
   (get-in model [::errors attribute])))

(defn get-errors
  "Returns the errors from the specified model. If given only a model, 
  returns a map of all errors. If given a model and a key, returns the 
  errors for the specified key from wihin the model."
  ([model]
   (->> (get model ::errors)
        (mapcat second)
        (into [])))
  ([model attr-key]
   (get-in model [::errors attr-key])))
