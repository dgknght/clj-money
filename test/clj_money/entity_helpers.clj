(ns clj-money.entity-helpers
  (:require [clojure.test :refer [is]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [clojure.walk :refer [postwalk]]
            [integrant.core :as ig]
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

    :else
    x))

(defn jsonify
  [data]
  (postwalk (fn [x]
              (if (entities/composite-id? x)
                (str x)
                x))
            data))

(defn capture-change
  "Invokes f with a publisher that captures the changes to entities of the
  given type, and returns f's result and the first change published."
  [entity-type f]
  (let [publisher (ig/init-key ::entities/publisher {})
        ch (entities/sub publisher [entity-type])]
    (try
      (let [result (f publisher)
            [change _ch] (a/alts!! [ch (a/timeout 1000)])]
        [result change])
      (finally
        (ig/halt-key! ::entities/publisher publisher)))))

(defn assert-created
  [attr & {:keys [refs
                  compare-result?
                  ignore-nils?
                  ignore-attributes]
           :or {refs []
                compare-result? true
                ignore-nils? false
                ignore-attributes []}}]
  (let [handle-nils (if ignore-nils? util/remove-nils identity)
        [result
         [before after]] (capture-change (util/entity-type attr)
                                         #(entities/put attr :publisher %))
        result (handle-nils result)
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
    (is (= result (handle-nils after))
        "The after (newly created) value is published")
    (is (nil? before)
        "The before value published is nil")
    fetched))

(defn assert-updated
  [entity attr]
  {:pre [entity attr]}
  (let [[result
         [before after]] (capture-change (util/entity-type entity)
                                         #(entities/put (merge entity attr)
                                                        :publisher %))]
    (is (comparable? attr result)
        "The return value contains the updated attributes")
    (is (comparable? attr (entities/find entity))
        "The retrieved value contains the updated attributes")
    (is (= entity before)
        "The before value is published")
    (is (= result after)
        "The after value is published")))

(defn assert-deleted
  [entity]
  {:pre [entity (:id entity)]}
  (let [[_ [before after]] (capture-change (util/entity-type entity)
                                           #(entities/delete entity :publisher %))]
    (is (nil? (entities/find entity))
        "The entity cannot be retrieved after being deleted")
    (is (= entity before)
        "The before value is published")
    (is (nil? after)
        "The after value published is nil")))

(defn assert-invalid
  [attr errors & {:keys [message put-opts]}]
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors errors}
        (apply entities/put attr (mapcat identity put-opts)))
      (or message "Expected a validation error, but found none.")))
