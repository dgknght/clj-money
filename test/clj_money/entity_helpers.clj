(ns clj-money.entity-helpers
  (:require [clojure.test :refer [is]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.entities :as entities]))

(derive clojure.lang.PersistentVector ::vector)

(declare simplify-refs)

(defn simplify-ref
  [m k]
  (if (vector? k)
    (update-in-if m (take 1 k) #(simplify-refs % (vec (rest k))))
    (update-in-if m [k] select-keys [:id])))

(defn- simplify-map-refs
  [entity refs]
  (reduce simplify-ref
          (update-vals entity #(simplify-refs % refs))
          refs))

(defn- simplify-vector-refs
  [entities refs]
  (mapv #(simplify-refs % refs) entities))

(defn simplify-refs
  [x refs]
  (cond
    (vector? x)
    (simplify-vector-refs x refs)

    (map? x)
    (simplify-map-refs x refs)

    (entities/composite-id? x)
    (str x)

    :else
    x))

(defn assert-created
  [attr & {:keys [refs
                  compare-result?
                  ignore-nils?
                  ignore-attributes]
           :or {refs []
                compare-result? true
                ignore-nils? false
                ignore-attributes []}}]
  (let [out-chan (a/chan)
        handle-nils (if ignore-nils? util/remove-nils identity)
        result (handle-nils (entities/put attr :out-chan out-chan))
        fetched (some-> result
                        :id
                        entities/find
                        handle-nils)
        expected  (apply dissoc
                         (handle-nils (simplify-refs attr refs))
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
  [entity attr]
  {:pre [entity attr]}
  (let [out-chan (a/chan)
        result (entities/put (merge entity attr)
                             :out-chan out-chan)]
    (is (comparable? attr result)
        "The return value contains the updated attributes")
    (is (comparable? attr (entities/find entity))
        "The retrieved value contains the updated attributes")
    (let [[[before after] _ch] (a/alts!! [out-chan (a/timeout 1000)])]
      (is (= entity before)
          "The before value is passed to the output channel")
      (is (= result after)
          "The after value is passed to the output channel"))))

(defn assert-deleted
  [entity]
  {:pre [entity (:id entity)]}
  (let [out-chan (a/chan)]
    (entities/delete entity :out-chan out-chan)
    (is (nil? (entities/find entity))
        "The entity cannot be retrieved after being deleted")
    (let [[[before after] _ch] (a/alts!! [out-chan (a/timeout 1000)])]
      (is (= entity before)
          "The before value is passed to the output channel")
      (is (nil? after)
          "The after value passed to the output channel is nil"))))

(defn assert-invalid
  [attr errors & {:keys [message]}]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (entities/put attr))
      (or message "Expected a validation error, but found none.")))
