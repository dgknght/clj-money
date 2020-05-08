(ns clj-money.inflection
  (:require [clojure.string :as string]))

(def ^:private title-case-ignore-patterns
  [#"^\s+$"
   #"^[A-Z]+$"])

(defn- title-case-ignore?
  [word]
  (some #(re-matches % word) title-case-ignore-patterns))

(defn- title-case-word
  [word]
  (if (title-case-ignore? word)
    word
    (string/capitalize word)))

(defn title-case
  "Renders the string in title case. E.g.
  (title-case \"my important thing\") => \"My Important Thing\""
  [s]
  (->> (-> s
           (string/replace "-" " ")
           (string/split #"\b"))
       (map title-case-word)
       (string/join "")))

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
  number
  (let [rules [{:pattern #"\A1[1-3]\z"
                :suffix "th"}
               {:pattern #"1\z"
                :suffix  "st"}
               {:pattern #"2\z"
                :suffix "nd"}
               {:pattern #"3\z"
                :suffix "rd"}
               {:pattern #"."
                :suffix "th"}]
        s (str number)]
    (str s (some #(when (re-find (:pattern %) s)
                    (:suffix %))
                 rules))))

(defn- apply-word-rule
  [word {pattern :pattern f :fn}]
  (when-let [match (re-find pattern word)]
    (f match)))

(defn singular
  "Accepts a plural noun and attempts to convert it into singular"
  [word]
  (let [rules [{:pattern #"(?i)\A(child)ren\z"
                :fn second}
               {:pattern #"(?i)\A(.+)ies\z"
                :fn #(str (second %) "y")}
               {:pattern #"(.+)s\z"
                :fn second}]]
    (some (partial apply-word-rule word) rules)))

(defn plural
  [word]
  (let [rules [{:pattern #"(?i)\Achild\z"
                :fn #(str % "ren")}
               {:pattern #"(?i)(.+)y\z"
                :fn #(str (second %) "ies")}
               {:pattern #".+"
                :fn #(str % "s")}]]
    (some (partial apply-word-rule word) rules)))
