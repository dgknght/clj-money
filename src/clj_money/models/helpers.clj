(ns clj-money.models.helpers
  (:require [clj-money.models.storage.sql-storage])
  (:import clj_money.models.storage.sql_storage.SqlStorage))

; TODO should this be dynamic to support extension?
(defn storage
  "Returns a storage implementation appropriate
  for the specified config"
  [config]
  (cond
    (= "postgresql" (:dbtype config)) (SqlStorage. config)))
