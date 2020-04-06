(ns clj-money.models.storage.sql-storage
  (:refer-clojure :exclude [update count])
  (:require [clojure.java.jdbc :as jdbc]
            [clj-postgresql.types]
            [stowaway.core :as storage :refer [Storage]]
            [clj-money.models :as models]))

(defn- dispatch-model
  [model & _]
  (models/tag model))

(defmulti select dispatch-model)
(defmulti insert dispatch-model)
(defmulti update dispatch-model)
(defmulti delete dispatch-model)

(deftype SqlStorage [db-spec]
  Storage

  (create
    [_ model]
    (insert model db-spec))

  (select
    [_ criteria options]
    (select criteria options db-spec))

  (update
    [_ model]
    (update model db-spec))

  (update
    [_ attr criteria]
    (update attr criteria db-spec))

  (delete
    [_ model]
    (delete model db-spec))

  ; Database Transaction
  (with-transaction
    [_ func]
    (jdbc/with-db-transaction [trans db-spec {:isolation :serializable :read-only false}]
      (func (SqlStorage. trans)))))

(storage/register-strategy
  (fn [storage-spec]
    (when (and (string? storage-spec)
               (re-find #"\Apostgres" storage-spec))
      (SqlStorage. storage-spec))))
