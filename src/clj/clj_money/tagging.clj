(ns clj-money.tagging)

(defn tag
  "Adds a tag to a model"
  [model tag]
  (update-in model [:tags] #((fnil conj #{}) % tag)))

(defn tagged?
  "Checks to see if a tag is present on a model"
  [model tag]
  (contains? (:tags model) tag))
