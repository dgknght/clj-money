(ns clj-money.coercion
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clj-money.util :refer [parse-local-date]])
  (:import java.util.UUID))

(def ^:private fns (atom {}))

(defn register-coerce-fn
  [key-name coerce-fn]
  (swap! fns #(assoc % key-name coerce-fn)))

(defn rule
  [rule-key path]
  {:path path
   :coerce-fn (or (rule-key @fns) 
                  (throw (RuntimeException. (format "No coercion function found for key %s" rule-key))))})

(defn rules
  [rule-key & paths]
  (map #(rule rule-key %) paths))

(defn parse-uuid-collection
  [values]
  (map #(if (instance? UUID %)
          %
          (UUID/fromString %))
       values))

(defmulti parse-integer-collection
  (fn [value]
    (cond
      (string? value) :string
      (and (coll? value) (every? integer? value)) :integer-collection
      (coll? value) :collection)))

(defmethod parse-integer-collection :string
  [value]
  [(try
     (Integer. value)
     (catch NumberFormatException e
       value))])

(defmethod parse-integer-collection :integer-collection
  [value]
  value)

(defmethod parse-integer-collection :collection
  [value]
  (map #(try
          (Integer. %)
          (catch NumberFormatException e
            %))
       value))

(register-coerce-fn :integer    #(if (integer? %)
                                   %
                                   (when (re-matches #"\A\d+\z" %)
                                     (Integer. %))))
(register-coerce-fn :decimal    #(if (decimal? %) % (try
                                                      (bigdec %)
                                                      (catch NumberFormatException e
                                                        %))))
(register-coerce-fn :keyword    #(if (keyword? %) % (keyword %)))
(register-coerce-fn :local-date #(if (string? %) (or (parse-local-date %) %) %))
(register-coerce-fn :integer-collection parse-integer-collection)
(register-coerce-fn :uuid-collection parse-uuid-collection)
(register-coerce-fn :uuid #(if (instance? UUID %) % (UUID/fromString %)))

(defn coerce
  "Given a model and a list of coercion rules, applies the rules to the model"
  [coercions model]
  (when-not (sequential? coercions)
    (throw (ex-info "The coercions must be a sequence" {:coercisons coercions})))
  (reduce (fn [result {:keys [path coerce-fn]}]
            (if (nil? (get-in result path))
              result
              (update-in result path coerce-fn)))
          model
          coercions))
