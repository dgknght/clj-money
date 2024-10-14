(ns clj-money.model-helpers
  (:require [clojure.test :refer [is]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.models :as models]))

(defn assert-created
  [attr & {:keys [refs] :or {refs []}}]
  (let [result (models/put attr)
        expected (reduce #(update-in-if %1 [%2] select-keys [:id])
                         attr
                         refs)]
    (is (comparable? expected result)
        "The result matches the input")
    (is (comparable? expected (models/find result))
        "The retrieved matches the input")
    result))

(defn assert-updated
  [model attr]
  {:pre [model attr]}
  (let [result (models/put (merge model attr))]
    (is (comparable? attr result)
        "The return value contains the updated attributes")
    (is (comparable? attr (models/find model))
        "The retrieved value contains the updated attributes")))

(defn assert-invalid
  [attr errors]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (models/put attr))))
