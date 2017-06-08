(ns clj-money.models.helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.models.storage :refer [with-transaction]]
            [clj-money.models.storage.sql-storage])
  (:import clj_money.models.storage.sql_storage.SqlStorage
           clj_money.models.storage.Storage))

;; Storage
; TODO should this be dynamic to support extension?
(def handlers
  [{:can-handle-fn #(and
                      (string? %)
                      (re-find #"\Apostgresql" %))
    :create-fn #(SqlStorage. %)}])

(defn- process-handler
  "Calls the specified check function to see if the handler
  can handle the specified config. If yes, then the create
  function is called to return the storage server. Otherwise
  nil is returned"
  [{:keys [can-handle-fn create-fn]} config]
  (when (can-handle-fn config)
    (create-fn config)))

(defn storage*
  "Returns a storage implementation appropriate
  for the specified config"
  [config]
  (when (not config)
    (throw (RuntimeException.
             "No configuration was specified. Unable to select a storage provider.")))
  (if (instance? Storage config)
    config

    (or (some #(process-handler % config) handlers)
        (throw (RuntimeException.
                 (format "Unable to find a storage provider for config %s" config))))))

(defmacro with-storage
  "Binds the 1st argument to an implementation
  of the Storage protocol and executes the body"
  [binding & body]
  `(let [s# (storage* ~(second binding))
         f# (fn* [~(first binding)] ~@body)]
     (f# s#)))

(defmacro with-transacted-storage
  "Evaluates the body in the context of a transaction using the configured
  storage mechanism.
  (transacted-storage [t-store storage-spec]
  ...do stuff with the t-store...)"
  [binding & body]
  `(let [s# (storage* ~(second binding))
         f# (fn* [~(first binding)] ~@body)]
     (with-transaction s# f#)))

(defn- validate
  [options s model]
  (let [rules (or (:rules options)
                  (when-let [rules-fn (:rules-fn options)]
                    ((:rules-fn options) s))
                  [])
        before-validation (or (:before-validation options)
                              (fn [_ m] (identity m)))
        coercion-rules (or (:coercion-rules options)
                           [])]
    (if (seq rules)
      (->> model
           (coercion/coerce coercion-rules)
           (before-validation s)
           (validation/validate (:spec options) rules))
      (assoc model :clj-money.validation/valid? true :clj-money.validation/errors []))))

(defn- process-options
  [options storage model & fn-keys]
  (->> fn-keys
       (map #(or
               (% options)
               (fn [_ model] model)))
       (reduce (fn [model f]
                 (f storage model))
               model)))

(defn create-fn
  [options]
  (fn [storage-spec model]
    (with-storage [s storage-spec]
      (let [validated (validate options s model)]
        (if (validation/valid? validated)
          (process-options options s validated :before-save :create :after-read)
          validated)))))

(defn update-fn
  [options]
  (fn [storage-spec model]
    (with-storage [s storage-spec]
      (let [validated (validate options s model)]
        (if (validation/valid? validated)
          (do
            (process-options options s validated :before-save :update)
            ((:find options) s (:id validated)))
          validated)))))
