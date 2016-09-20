(ns clj-money.inflection
  (:require [clojure.string :as string]))

(defn humanize
  "Accepts a value in kabob case and returns the value in human friendly form"
  [value]
  (-> value
      name
      (string/replace "-" " ")
      string/capitalize))
