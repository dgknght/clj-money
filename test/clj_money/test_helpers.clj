(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing]]
            [java-time.api :as t]
            [clj-money.config :refer [env]]
            [dgknght.app-lib.test :as test]
            [clj-money.decimal :as d]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.threading :refer [thread-db-index
                                         thread-specific-config
                                         with-db-lock]]
            [clj-money.entities :as entities]))

; TODO: Remove this an just use the reset in the dbtest so that we don't have to duplicate the strategy selection logic
(def active-db-config
  (get-in env [:db :strategies (get-in env [:db :active])]))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (let [idx (thread-db-index)
        config (thread-specific-config active-db-config idx)]
    (with-db-lock idx
      (db/with-storage [config]
        (db/reset (db/storage))
        (f)))))

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

(defn include-strategy
  "Builds a function that evaluates options passed into dbtest that will
  return true if the test should be execute and false if not."
  [{:keys [only exclude]}]
  (cond
    only    (list 'clj-money.test-helpers/->set only)
    exclude `(complement ~(clj-money.test-helpers/->set exclude))
    :else   '(constantly true)))

(def isolate (when-let [isolate (env :isolate)]
               #{(if (string? isolate)
                   (keyword isolate)
                   isolate)}))

(def ignore-strategy
  "A predicate that ignores a strategy based on the environment
  variable IGNORE_STRATEGY, like 'IGNORE_STRATEGY=sql'"
  (if isolate
                       (complement isolate)
                       (if-let [ignore (env :ignore-strategy)]
                         (->set ignore)
                         (constantly false))))

(def honor-strategy (complement ignore-strategy))

(defmacro dbtest
  "Executes the body against all configured db strategies"
  [test-name & body]
  (let [strategies (-> env :db :strategies)]
    `(do
       ~@(for [[strategy-name config] strategies]
           (let [q-test-name (with-meta
                               (symbol (str (name test-name)
                                            "-"
                                            (name strategy-name)))
                               {:strategy strategy-name})]
             `(deftest ~q-test-name
                (let [idx# (thread-db-index)
                      thread-config# (thread-specific-config
                                       ~config
                                       idx#)]
                  (with-db-lock idx#
                    (db/with-storage [thread-config#]
                      (db/reset (db/storage))
                      ~@body)))))))))
