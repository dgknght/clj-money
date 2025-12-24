(ns clj-money.threading
  (:require [clj-money.db :as db]))

(defn- available-processors []
  (.availableProcessors (Runtime/getRuntime)))

(defn- thread-id []
  (.getId (Thread/currentThread)))

(def db-locks
  "Vector of locks, one per database index"
  (vec (repeatedly (available-processors)
                   #(Object.))))

(def ^:private thread-index-counter (atom 0))
(def ^:private thread-indices (atom {}))

(defn thread-db-index
  "Returns a database index for the current thread. Each unique thread
  gets assigned the next available index (cycling through available
  processors). This ensures each thread consistently uses the same
  database instance throughout the test run."
  []
  (let [id (thread-id)
        processor-count (available-processors)]
    (or (@thread-indices id)
        (let [idx (mod (swap! thread-index-counter inc) processor-count)]
          (swap! thread-indices assoc id idx)
          idx))))

(defmacro with-db-lock
  "Acquires a lock for the database at the given index, executes body,
  and releases the lock. Prevents concurrent access to the same database."
  [idx & body]
  `(locking (nth db-locks ~idx)
     ~@body))

(defmulti thread-specific-config
  "Creates a thread-specific database configuration to allow parallel test execution"
  (fn [c _] (::db/strategy c)))

(defmethod thread-specific-config ::db/datomic-peer
  [config idx]
  (update-in config
             [:uri]
             #(format "%s_%s"
                      %
                      idx)))

(defmethod thread-specific-config ::db/sql
  [config idx]
  (update-in config
             [:dbname]
             #(format "%s_%s"
                      %
                      idx)))
