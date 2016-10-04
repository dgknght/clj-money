(ns clj-money.models.helpers
  (:require [clojure.pprint :refer [pprint]]
            [schema.core :as schema]
            [schema.coerce :as coerce]
            [schema.utils :as sutils]
            [clj-money.models.storage]
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

(defn storage
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

;; Validation
(defn- int-matcher
  [schema]
  (when (= schema/Int schema)
    (coerce/safe
      (fn [value]
        (if (and (string? value) (re-matches #"\d+" value))
          (Integer. value)
          value)))))

(defn- nil-matcher
  [schema]
  (when (= schema/Str schema)
    (coerce/safe
      (fn [value]
        (if (and (string? value) (= 0 (count value)))
          nil
          value)))))

(defn throw-validation-exception
  "Throws a validation exception that is consistent with
  what is thrown by prismatic schema"
  [errors model schema model-name]
  (throw (ex-info (format "The %s is not valid." model-name)
                      (assoc {:schema schema
                              :value model
                              :type :schema.core/error}
                             :error errors))))

(defn validate-model
  "Coerces and validates the specified model using the specified schema"
  [model schema model-name]
  (let [coercer (coerce/coercer schema
                                (coerce/first-matcher [int-matcher
                                                       nil-matcher
                                                       coerce/json-coercion-matcher]))
        result (coercer model)]
    (if (sutils/error? result)
     (throw-validation-exception (:error result) model schema model-name)
      result)))
