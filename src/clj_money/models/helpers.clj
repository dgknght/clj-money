(ns clj-money.models.helpers
  (:require [clj-money.models.storage.sql-storage])
  (:import clj_money.models.storage.sql_storage.SqlStorage))

; TODO should this be dynamic to support extension?
(def handlers
  [{:can-handle-fn #(= "postgresql" (:dbtype %))
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
  (some #(process-handler % config) handlers))
