(ns clj-money.inflection
  (:require [clojure.string :as string])
  )

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
  (->> (string/split s #"\b")
       (map title-case-word)
       (string/join "")))
