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

(defn last-segment
  [value]
  (last (string/split value #"\.")))
