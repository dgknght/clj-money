(ns clj-money.validation
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [parse-date]]
            [clj-money.inflection :refer [singular
                                          humanize
                                          ordinal]])
  (:import org.joda.time.LocalDate
           java.util.Date))

(defn non-empty-string?
  [value]
  (and (string? value)
       (pos? (count value))))

(defn- interpret-empty-string-failure
  [problem]
  (when (and (symbol? (:pred problem))
             (= 'non-empty-string? (:pred problem)))
    (let [attribute (-> problem :path first)]
      [attribute (format "%s cannot be empty" (humanize attribute))])))

(defn- interpret-regex-failure
  [problem]
  (when (and (coll? (:pred problem))
             (= 're-matches (-> problem :pred second)))
    (let [attribute (-> problem :path first)]
      [attribute (format "%s is not valid" (humanize attribute))])))

(defn- interpret-required-failure
  [problem]
  (when (and (coll? (:pred problem))
             (= 'contains? (-> problem :pred first)))
    (let [attribute (-> problem :pred (nth 2))]
      [attribute (format "%s is required" (humanize attribute))])))

(defn- interpret-set-inclusion-failure
  [problem]
  (when (set? (:pred problem))
    (let [attribute (-> problem :path first)]
      [attribute
       (format "%s must be one of: %s"
               (humanize attribute)
               (->> problem
                    :pred
                    (map name)
                    (string/join ", ")))])))

(def problem-interpreters
  [interpret-required-failure
   interpret-regex-failure
   interpret-empty-string-failure
   interpret-set-inclusion-failure])

(defn- problem->message
  [problem]
  (or (some #(% problem) problem-interpreters)
      (throw (RuntimeException. (str "Unable to make sense of the problem: " problem)))))

(defn- interpret-problems
  [explanation]
  (->> explanation
       :clojure.spec/problems
       (map problem->message)
       (reduce (fn [result [k message]]
                 (update-in result [k] (fnil #(conj % message) [])))
               {})))

(defn- perform-additional-validation
  "Performs validation that is not suitable for clojure.spec. E.g., database checks
  for unique values."
  [model rules]
  (let [violations (->> rules
                        (remove #(% model))
                        (map (comp (juxt ::path ::message) meta))
                        (reduce (fn [result [path message]]
                                  (update-in result path (fnil #(conj % message) [])))
                                {}))]
    (if (seq violations)
      (assoc model ::valid? false ::errors violations)
      (assoc model ::valid? true))))

(defmacro create-rule
  "Given a predicate function an error message and a map path,
  returns a validation rule that can be passed to the validate function"
  [pred path message]
  `(vary-meta ~pred assoc ::path ~path ::message ~message))

(defn validate
  "Validates the specified model using the specified spec"
  [spec model & rules]
  (if-let [explanation (s/explain-data spec model)]
    (assoc model ::errors (interpret-problems explanation)
           ::valid? false)
    (perform-additional-validation model rules)))

(defn has-error?
  "Returns true if the specified model contains validation errors"
  ([model]
   (contains? model ::errors))
  ([model attribute]
   (get-in model [::errors attribute])))

(defn valid?
  "Returns false if the model has any validation errors"
  [model]
  (::valid? model))

(defn error-messages
  "Returns the errors from the specified model. If given only a model, 
  returns a map of all errors. If given a model and a key, returns the 
  errors for the specified key from wihin the model."
  ([model]
   (::errors model))
  ([model attribute]
   (attribute (error-messages model))))
