(ns clj-money.validation
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [schema.core :as schema]
            [schema.coerce :as coerce]
            [schema.utils :as schema-utils]
            [clj-time.core :as t]
            [clj-money.inflection :refer [singular
                                          humanize
                                          ordinal]]
            [clj-money.schema :refer [friendly-message]])
  (:import org.joda.time.LocalDate))

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
                                            (let [error-list (map second tuples)
                                                  error-list (if (vector? (k model))
                                                                          (first error-list)
                                                                          error-list)]
                                              [k error-list])))
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

(def date-patterns
  [{:pattern #"(\d{1,2})/(\d{1,2})/(\d{4})"
    :groups [:month :day :year]}
   {:pattern
    #"(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})"
    :groups [:year :month :day]}])

(defn- local-date-matcher
  [schema]
  (when (= LocalDate schema)
    (coerce/safe
      (fn [value]
        (if (string? value)
          (when-let [parsed (some (fn [{:keys [pattern groups]}]
                                    (when-let [m (re-matches pattern value)]
                                      (zipmap groups (->> m
                                                          rest
                                                          (map #(Integer. %))))))
                                  date-patterns)]
            (apply t/local-date ((juxt :year :month :day) parsed)))
          value)))))

(defn- full-humanized-message
  "Accepts a tuple containg an attribute key and a schema
  violation expresion and returns a human-friendly message"
  [[attr-key expr]]
  (let [humanized (if (vector? expr)
                    (vec (map #(when %
                                 (into {} (map full-humanized-message %))) expr))
                    (str (humanize attr-key) " " (friendly-message expr)))]
    [attr-key humanized]))

(defn apply-schema
  "A rule function that coerces and applies a schema to a model"
  [schema model]
  (let [coercer (coerce/coercer schema
                                (coerce/first-matcher [int-matcher
                                                       local-date-matcher
                                                       nil-matcher
                                                       coerce/json-coercion-matcher]))
        result (coercer model)
        errors (if (schema-utils/error? result)
               (vec (map full-humanized-message
                         (schema-utils/error-val result)))
               [])]
    {:model (dissoc result :error)
     :errors errors}))

(defn has-error?
  "Returns true if the specified model contains validation errors"
  ([model]
   (contains? model ::errors))
  ([model attribute]
   (get-in model [::errors attribute])))

(defn valid?
  "Returns false if the model has any validation errors"
  [model]
  (not (has-error? model)))

(defn- extract-message
  "Accepts a validation error key/value pair 
  performs any necessary adjustments to make it readable
  and the current level of nesting"
  [[k v]]
  (if (-> v first string?)
    v
    (filter identity
            (map-indexed (fn [index err]
                           (when err
                             (str (ordinal (+ 1 index)) " " (singular (humanize k)) ": " (string/join ", " (map second (seq err))))))
                         v))))

(defn get-errors
  "Returns the errors from the specified model. If given only a model, 
  returns a map of all errors. If given a model and a key, returns the 
  errors for the specified key from wihin the model."
  [model & attr-keys]
  (if (seq attr-keys)
    (get-in model (concat [::errors] attr-keys))
    (->> (get model ::errors)
         (mapcat #(extract-message %))
         vec)))
