(ns clj-money.validation
  (:require [dgknght.app-lib.forms-validation :as v]))

(defn drop-rules
  "Validation rules stay in the model's metadata after their field is
  unmounted, so a form that swaps fields in and out has to remove the
  rules for the fields it isn't showing before it validates the model"
  [model fields]
  (when (seq fields)
    (swap! model
           vary-meta
           update-in
           [::v/validation ::v/rules]
           #(apply dissoc % fields))))
