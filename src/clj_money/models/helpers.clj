(ns clj-money.models.helpers
  (:require [slingshot.slingshot :refer [throw+]]))

(defn throw-if-nil
  "If the value is nil, throws an exception indicating the
  model could not be found, otherwise returns the value
  for use in threading"
  [value]
  (if value
    value
    (throw+ {:type :clj-money.models/not-found})))
