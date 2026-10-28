(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest]]
            [java-time.api :as t]
            [integrant.core :as ig]
            [clj-money.config :refer [env]]
            [dgknght.app-lib.test :as test]
            [clj-money.ingestion.mock]
            [clj-money.prices.mock]
            [clj-money.decimal :as d]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            ; loads the implementations of prop/propagate, which must be
            ; loaded before a propagation subscription starts
            [clj-money.entities.ref]))

; Storage systems, keyed by db config, initialized on first use and
; reused by every subsequent test against the same database.
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

(def ^:dynamic *propagation*
  "The propagation subscription started for the current test"
  nil)

(defn await-propagation
  "Blocks until every change published so far in the current test has been
  propagated. Throws if propagation doesn't finish in time."
  []
  (when *propagation*
    (when-not (prop/await-propagation *propagation*)
      (throw (ex-info "Timed out waiting for propagation" {}))))
  nil)

(defn propagated
  "Returns x once every change published so far in the current test has
  been propagated, e.g.:

    (propagated (entities/put transaction))"
  [x]
  (await-propagation)
  x)

(defn put-and-propagate
  "Puts the entity and waits for the change to be propagated"
  [entity & {:as opts}]
  (propagated (entities/put entity opts)))

(defn delete-and-propagate
  "Deletes the entity and waits for the change to be propagated"
  [entity & {:as opts}]
  (propagated (entities/delete entity opts)))

(defn with-propagation*
  "Invokes f with a publisher and a propagation subscription started and
  bound, so that changes are propagated as they are in the running
  application. Propagation finishes before this function returns."
  [storage f]
  (let [publisher (ig/init-key ::entities/publisher {})
        subscription (ig/init-key ::prop/subscription
                                  {:storage storage
                                   :publisher publisher})]
    (try
      (binding [entities/*publisher* publisher
                *propagation* subscription]
        (f))
      (finally
        ; finish propagating before the test ends, so that nothing is
        ; written to the database after it's reset for the next test
        (prop/await-propagation subscription)
        (ig/halt-key! ::prop/subscription subscription)
        (ig/halt-key! ::entities/publisher publisher)))))

(defn with-test-storage
  "Resets the storage for the given db config and invokes f with that
  storage bound, and with changes propagated (see with-propagation*)."
  [config f]
  (let [storage (test-storage config)]
    (binding [db/*storage* storage]
      (db/reset storage)
      (with-propagation* storage f))))

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
