(ns clj-money.validation
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
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
  [{:keys [path pred]}]
  (when (and (symbol? pred)
             (= 'non-empty-string? pred))
    [path (format "%s cannot be empty" (humanize (last path)))]))

(defn positive-big-dec?
  [value]
  (and (instance? BigDecimal value)
       (pos? value)))

(defn- interpret-positive-big-dec-failure
  [{:keys [path pred]}]
  (when (and (symbol? pred)
             (= 'positive-big-dec? pred))
    [path (format "%s must be a positive number" (humanize (last path)))]))

(defn- interpret-regex-failure
  [{:keys [path pred]}]
  (when (and (coll? pred)
             (= 're-matches (second pred)))
    [path (format "%s is not valid" (humanize (last path)))]))

(defn- interpret-required-failure
  [{:keys [path pred in] :as problem}]
  (when (and (coll? pred)
             (= 'contains? (first pred)))
    [(concat in [(nth pred 2)]) (format "%s is required" (humanize (nth pred 2)))]))

(defn- interpret-set-inclusion-failure
  [{:keys [pred path]}]
  (when (set? pred)
    [path
     (format "%s must be one of: %s"
             (humanize (last path))
             (->> pred
                  (map name)
                  (string/join ", ")))]))

(defn- interpret-collection-count-failure
  [{:keys [pred path]}]
  (when (and (seq? pred)
             (or (= (first pred) '<=)
                 (= (first pred) 'clojure.core/<=)))
    [path
     (format "Count must be greater than or equal to %s" (second pred))]))

(defn- interpret-unknown-failure
  [{:keys [pred path] :as problem}]
  (pprint {:interpret-unknown-failure problem
           :class (class pred)})
  [path (format "The attribute at %s is not valid" path)])

(defn- interpret-type-failure
  [{:keys [pred path] :as problem}]
   (when (and (seq pred)
              (= 'instance? (second pred)))
     [path (format "%s must be an instance of %s" (-> path last humanize) (nth pred 2))]))

(def problem-interpreters
  [interpret-required-failure
   interpret-regex-failure
   interpret-empty-string-failure
   interpret-positive-big-dec-failure
   interpret-collection-count-failure
   interpret-set-inclusion-failure
   interpret-type-failure
   interpret-unknown-failure])

(defn- problem->message
  [problem]
  (some #(% problem) problem-interpreters))

(defn- interpret-problems
  [explanation]
  (->> explanation
       :clojure.spec/problems
       (map problem->message)
       (reduce (fn [result [k message]]
                 (update-in result k (fnil #(conj % message) [])))
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
