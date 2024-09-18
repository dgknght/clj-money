(ns clj-money.models.sql-storage
  (:refer-clojure :exclude [update count])
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clj-postgresql.types]
            [stowaway.core :as storage :refer [Storage]]))

(defn- dispatch-model
  [model & _]
  (storage/tag model))

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
