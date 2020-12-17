(ns clj-money.validation
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clj-money.inflection :refer [humanize]])
  (:import org.joda.time.LocalDate))

(def EmailPattern #"\A[\w\.-_]+@[\w\.-_]+\.\w{2,4}\z")

(defn email?
  [value]
  (and (string? value)
       (re-matches EmailPattern value)))

(defn local-date?
  [value]
  (instance? LocalDate value))

(defn non-empty-string?
  [value]
  (and (string? value)
       (pos? (count value))))

(defn positive-integer?
  [value]
  (and (integer? value)
       (pos? value)))

(defn positive-big-dec?
  [value]
  (and (decimal? value)
       (pos? value)))

(defn big-dec-not-less-than-zero?
  [value]
  (and (decimal? value)
       (or (pos? value)
           (zero? value))))

(def ^:private simple-pred-map
  {'clj-money.validation/local-date? "%s must be a date"
   'clj-money.validation/email? "%s must be a valid email"
   'clj-money.validation/positive-integer? "%s must be greater than zero"
   'clj-money.validation/positive-big-dec? "%s must be a positive number"
   'clj-money.validation/non-empty-string? "%s cannot be empty"
   'clojure.core/integer? "%s must be an integer"
   'clojure.core/decimal? "%s must be a decimal"})

(defn- interpret-simple-pred-failure
  [{:keys [path pred]}]
  (when (symbol? pred)
    (when-let [fmt (simple-pred-map pred)]
      [path (format fmt (-> path last humanize))])))

(defn- interpret-regex-failure
  [{:keys [path pred]}]
  (when (and (coll? pred)
             (= 're-matches (second pred)))
    [path (format "%s is not valid" (humanize (last path)))]))

(defn- interpret-required-failure
  [{:keys [pred in]}]
  (when (and (coll? pred)
             (not (set? pred))
             (coll? (last pred))
             (= 'clojure.core/contains? (->> pred last first)))
    (let [k (->> pred last last)]
      [(concat in [k]) (format "%s is required" (humanize k))])))

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
  [{:keys [path] :as problem}]

  (log/debug "interpret-unknown-failure " problem)

  [path (format "The attribute at %s is not valid" path)])

(defn- interpret-type-failure
  [{:keys [pred path] :as problem}]
  (when (and (seq? pred)
             (= 'clojure.core/instance? (second pred)))
    (let [humanized (-> path last humanize)
          message (if (:val problem)
                    (format "%s must be an instance of %s" humanized (nth pred 2))
                    (format "%s is required" humanized))]
      [path message])))

(def problem-interpreters
  [interpret-required-failure
   interpret-simple-pred-failure
   interpret-regex-failure
   interpret-collection-count-failure
   interpret-set-inclusion-failure
   interpret-type-failure
   interpret-unknown-failure])

(defn- problem->message
  [problem]
  (some #(% problem) problem-interpreters))

; This isn't a perfect solution, but it accounts
; for the issues I've seen so far
(defn- flatten-multi-spec-paths
  [problem]
  (if (and (< 1 (count (:via problem)))
           (< 1 (count (:path problem)))
           (= (count (:via problem))
              (count (:path problem)))) ; this is getting very hacky
    (-> problem
        (update-in [:path] rest)
        (update-in [:via] rest))
    problem))

(defn- interpret-problems
  [explanation]
  (->> explanation
       :clojure.spec.alpha/problems
       (map (comp problem->message flatten-multi-spec-paths))
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
  ([model spec] (validate model spec []))
  ([model spec rules]
   (if-let [explanation (s/explain-data spec model)]
     (do (log/debugf "validation error %s: %s" (prn-str model) (prn-str explanation))
         (assoc model ::errors (interpret-problems explanation)
                ::valid? false))
     (perform-additional-validation model rules))))

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

(defn flat-error-messages
  "Returns a flat list of strings describing the error messages for the
  model instead of the map returned by error-messages"
  [model]
  (-> model
      ::errors
      vals
      flatten))

(defmacro with-validation
  [model spec rules & body]
  `(let [validated# (clj-money.validation/validate ~model ~spec ~rules)
         f# (fn* [~model] ~@body)]
     (if (clj-money.validation/valid? validated#)
       (f# validated#)
       validated#)))
