(ns clj-money.model-helpers
  (:require [clojure.test :refer [is]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.models :as models]))

(derive clojure.lang.PersistentVector ::vector)

(defmulti ^:private simplify-refs util/type-dispatch)

(defmethod simplify-refs :default
  [x _]
  x)

(defmulti ^:private simplify-ref
  (fn [_ k] (type k)))

(defmethod simplify-ref :default
  [m k]
  (update-in-if m [k] select-keys [:id]))

(defmethod simplify-ref ::vector
  [m ks]
  (update-in-if m (take 1 ks) #(simplify-refs % (vec (rest ks)))))

(defmethod simplify-refs ::util/map
  [model refs]
  (reduce simplify-ref
          (update-vals model #(simplify-refs % refs))
          refs))

(defmethod simplify-refs ::util/vector
  [models refs]
  (mapv #(simplify-refs % refs) models))

(defn assert-created
  [attr & {:keys [refs compare-result? ignore-attributes]
           :or {refs []
                compare-result? true
                ignore-attributes []}}]
  (let [out-chan (a/chan)
        result (models/put attr :out-chan out-chan)
        fetched (models/find result)
        expected  (apply dissoc
                         (simplify-refs attr refs)
                         ignore-attributes)]
    (when compare-result?
      (is (comparable? expected result)
          "The result matches the input"))
    (is (:id result)
        "The result contains an :id attribute")
    (is (comparable? expected fetched)
        "The retrieved matches the input")
    (let [[[before after] _ch] (a/alts!! [out-chan (a/timeout 1000)])]
      (is (= result after)
          "The after (newly created) value is passed to the output channel")
      (is (nil? before)
          "The before value passed to the output channel is nil"))
    fetched))

(defn assert-updated
  [model attr]
  {:pre [model attr]}
  (let [out-chan (a/chan)
        result (models/put (merge model attr)
                           :out-chan out-chan)]
    (is (comparable? attr result)
        "The return value contains the updated attributes")
    (is (comparable? attr (models/find model))
        "The retrieved value contains the updated attributes")
    (let [[[before after] _ch] (a/alts!! [out-chan (a/timeout 1000)])]
      (is (= model before)
          "The before value is passed to the output channel")
      (is (= result after)
          "The after value is passed to the output channel"))))

(defn assert-deleted
  [model]
  {:pre [model (:id model)]}
  (let [out-chan (a/chan)]
    (models/delete model :out-chan out-chan)
    (is (nil? (models/find model))
        "The model cannot be retrieved after being deleted")
    (let [[[before after] _ch] (a/alts!! [out-chan (a/timeout 1000)])]
      (is (= model before)
          "The before value is passed to the output channel")
      (is (nil? after)
          "The after value passed to the output channel is nil"))))

(defn assert-invalid
  [attr errors & {:keys [message]}]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (models/put attr))
      (or message "Expected a validation error, but found none.")))
