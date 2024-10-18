(ns clj-money.model-helpers
  (:require [clojure.test :refer [is]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.models :as models]))

(defmulti ^:private simplify-refs util/type-dispatch)

(defmethod simplify-refs :default
  [x _]
  x)

(defmethod simplify-refs ::util/map
  [model refs]
  (reduce #(update-in-if %1 [%2] select-keys [:id])
          (update-vals model #(simplify-refs % refs))
          refs))

(defmethod simplify-refs ::util/vector
  [models refs]
  (mapv #(simplify-refs % refs) models))

(defn assert-created
  [attr & {:keys [refs] :or {refs []}}]
  (let [result (models/put attr)
        expected (simplify-refs attr refs)]
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

(defn assert-deleted
  [model]
  {:pre [model (:id model)]}
  (models/delete model)
  (is (nil? (models/find model))
      "The model cannot be retrieved after being deleted"))

(defn assert-invalid
  [attr errors]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (models/put attr))))
