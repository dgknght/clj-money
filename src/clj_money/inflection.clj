(ns clj-money.inflection
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(defn humanize
  "Accepts a value in kabob case and returns the value in human friendly form"
  [value]
  (-> value
      name
      (string/replace #"[_-]" " ")
      string/capitalize))

(defn ordinal
  "Accepts a number and returns a string expressing the value
  as an ordinal. E.g., 1 => '1st'"
  [number]
  (let [rules [{:pattern #"(?<!1)1\z"
                :suffix  "st"}
               {:pattern #"(?<!1)2\z"
                :suffix "nd"}
               {:pattern #"(?<!1)3\z"
                :suffix "rd"}
               {:pattern #"."
                :suffix "th"}]
        s (str number)]
    (str s (some #(when (re-find (:pattern %) s)
                    (:suffix %))
                 rules))))

(defn- apply-singularize-rule
  [word {pattern :pattern f :fn}]
  (when-let [match (re-find pattern word)]
    (f match)))

(defn singular
  "Accepts a plural noun and attempts to convert it into singular"
  [word]
  (let [rules [{:pattern #"(?i)\A(child)ren\z"
                :fn second}
               {:pattern #"(.+)s\z"
                :fn second}]]
    (some (partial apply-singularize-rule word) rules)))
