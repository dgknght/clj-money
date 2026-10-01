(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest]]
            [java-time.api :as t]
            [integrant.core :as ig]
            [clj-money.config :refer [env]]
            [dgknght.app-lib.test :as test]
            [clj-money.decimal :as d]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.threading :refer [thread-db-index
                                         thread-specific-config
                                         with-db-lock]]
            [clj-money.entities :as entities]))

(def ^:dynamic *parallel* false)

; Storage systems, keyed by db config, initialized on first use and
; reused by every subsequent test against the same database. In parallel
; mode each thread index has its own config, and therefore its own system.
(def ^:private systems (atom {}))

(defn- test-storage
  [config]
  (-> systems
      (swap! (fn [m]
               (if (contains? m config)
                 m
                 (assoc m config (delay (ig/init {::db/storage config}))))))
      (get config)
      deref
      ::db/storage))

(defn halt-storage!
  "Halts every storage system initialized by the test harness"
  []
  (let [[halting _] (reset-vals! systems {})]
    (doseq [sys (vals halting)
            :when (realized? sys)]
      (ig/halt! @sys))))

(defonce halt-storage-on-shutdown
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. ^Runnable halt-storage!)))

(defn- call-with-storage
  [config f]
  (let [storage (test-storage config)]
    (binding [db/*storage* storage]
      (db/reset storage)
      (f))))

(defn with-test-storage
  "Resets the storage for the given db config and invokes f with that
  storage bound. In parallel mode, the config is adjusted for the
  current thread's database, which is locked for the duration of f."
  [config f]
  (if *parallel*
    (let [idx (thread-db-index)]
      (with-db-lock idx
        (call-with-storage (thread-specific-config config idx) f)))
    (call-with-storage config f)))

(defn reset-db
  "Deletes all records from all tables in the active database prior to
  test execution"
  [f]
  (with-test-storage (db/active-config env) f))

(defn- throw-if-nil
  [x msg]
  (when (nil? x)
    (throw (ex-info msg {})))
  x)

(defn account-ref
  [name]
  (-> {:account/name name}
      entities/find-by
      (throw-if-nil (str "Account not found: " name))
      util/->entity-ref))

(defn parse-edn-body
  [res]
  (test/parse-edn-body res :readers {'clj-money/local-date t/local-date
                                     'clj-money/local-date-time t/local-date-time
                                     'clj-money/decimal d/d}))

(defn ->set
  [v]
  (if (coll? v)
    (set v)
    #{v}))

(defn include-strategy?
  [{:keys [only except]}]
  (cond
    only   (comp (->set only) first)
    except (complement (comp (->set except) first))
    :else  (constantly true)))

(defmacro dbtest
  "Executes the body against all configured db strategies"
  [test-name & body]
  (let [mdata (meta test-name)
        strategy-names (->> (-> env :db :strategies)
                            (filter (include-strategy? mdata))
                            (map key))]
    `(do
       ~@(for [strategy-name strategy-names]
           (let [q-test-name (with-meta
                               (symbol (str (name test-name)
                                            "-"
                                            (name strategy-name)))
                               (-> mdata
                                   (dissoc :only :except)
                                   (merge {:strategy strategy-name})))]
             `(deftest ~q-test-name
                (with-test-storage
                  (get-in env [:db :strategies ~strategy-name])
                  (fn [] ~@body))))))))
