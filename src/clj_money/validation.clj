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

(defn validate
  "Validates the specified model using the specified rules.
  If any violations are found, they are added to the model
  in a sequence under the key :clj-money.validation/errors.
  
  A rule is simply a function that accepts the model and
  returns truthy if the rule is satisfied and falsey if it is not.

  Additionally the rule has meta data necessary to construct the
  error messages.

  (defn ^{:clj-money.validation/message \"First name is a required field\"
          :clj-money.validation/attribute :first-name}
    first-name-is-present
    [model]
    (:first-name model))"
  [spec model]
  (let [explanation (s/explain-data spec model)]
    (if explanation
      (assoc model ::errors explanation)
      (assoc model ::valid true))))

(defn has-error?
  "Returns true if the specified model contains validation errors"
  ([model]
   (contains? model ::errors))
  ([model attribute]
   (get-in model [::errors attribute])))

(defn valid?
  "Returns false if the model has any validation errors"
  [model]
  (::valid model))

(defn- problem->message
  [problem]
  (let [attribute (-> problem :pred (nth 2))]
    [attribute
     (if (= 'contains? (-> problem :pred first))
       (str (humanize attribute) " is required")
       "Something ain't right.")]))

(defn error-messages
  "Returns the errors from the specified model. If given only a model, 
  returns a map of all errors. If given a model and a key, returns the 
  errors for the specified key from wihin the model."
  ([model]
   (->> model
        ::errors
        :clojure.spec/problems
        (map problem->message)
        (reduce (fn [result [k message]]
                  (update-in result [k] (fnil #(conj % message) [])))
                {})))
  ([model attribute]
   (attribute (error-messages model))))
