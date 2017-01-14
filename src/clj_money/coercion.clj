(ns clj-money.coercion
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [parse-local-date]]))

(def ^:private fns (atom {}))

(defn register-coerce-fn
  [key-name coerce-fn]
  (swap! fns #(assoc % key-name coerce-fn)))

(defn rule
  [rule-key path]
  {:path path
   :coerce-fn (or (rule-key @fns) 
                  (throw (RuntimeException. (format "No coercion function found for key %s" rule-key))))})

(register-coerce-fn :integer    #(if (integer? %) % (Integer. %)))
(register-coerce-fn :decimal    #(if (decimal? %) % (bigdec %)))
(register-coerce-fn :keyword    #(if (keyword? %) % (keyword %)))
(register-coerce-fn :local-date #(if (string? %) (parse-local-date %) %))

(defn coerce
  "Given a model and a list of coercion rules, applies the rules to the model"
  [model coercions]
  (reduce (fn [result {:keys [path coerce-fn]}]
            (if (nil? (get-in result path))
              result
              (update-in result path coerce-fn)))
          model
          coercions))
