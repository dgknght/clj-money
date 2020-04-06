(ns clj-money.models.helpers
  (:require [slingshot.slingshot :refer [throw+]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [stowaway.core :refer [with-storage]]))

(defn- validate
  [model options s]
  (let [rules (or (:rules options)
                  (when-let [rules-fn (:rules-fn options)]
                    (rules-fn s))
                  [])
        before-validation (or (:before-validation options)
                              (fn [m & _] (identity m)))
        coercion-rules (or (:coercion-rules options)
                           [])]
    (-> model
        (coercion/coerce coercion-rules)
        (before-validation s)
        (validation/validate (:spec options) rules))))

(defn- process-options
  [options storage model & fn-keys]
  (->> fn-keys
       (map #(% options))
       (keep identity)
       (reduce (fn [model f]
                 (f model storage))
               model)))

(defn create-fn
  [options]
  (fn [storage-spec model]
    (with-storage [s storage-spec]
      (let [validated (validate model options s)]
        (if (validation/valid? validated)
          (process-options options s validated
                           :before-save
                           :create
                           :after-save
                           :after-read)
          validated)))))

(defn update-fn
  [options]
  (fn [storage-spec model]
    (with-storage [s storage-spec]
      (let [validated (validate model options s)]
        (if (validation/valid? validated)
          (do
            (process-options options s validated :before-save :update)
            (if (:find options)
              ((:find options) s (:id validated))
              ((:reload options) s validated)))
          validated)))))

(defn throw-if-nil
  "If the value is nil, throws an exception indicating the
  model could not be found, otherwise returns the value
  for use in threading"
  [value]
  (if value
    value
    (throw+ {:type :clj-money.models/not-found})))
