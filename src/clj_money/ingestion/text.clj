(ns clj-money.ingestion.text
  "Compares the text read from documents with the text expected or
  already recorded"
  (:require [clojure.string :as str]))

(defn- tokens
  [s]
  (->> (str/split (str/lower-case (str s)) #"[^a-z0-9]+")
       (remove str/blank?)
       (map #(get {"street" "st"
                   "road" "rd"
                   "drive" "dr"
                   "suite" "ste"
                   "texas" "tx"}
                  %
                  %))
       set))

(defn similarity
  "Returns the share of words the two strings have in common, from 0 to 1."
  [a b]
  (let [ta (tokens a)
        tb (tokens b)]
    (if (and (seq ta) (seq tb))
      (/ (count (filter ta tb))
         (double (count (into ta tb))))
      0.0)))
