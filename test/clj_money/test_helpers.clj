(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest]]
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

(defn include-strategy?
  [{:keys [only except]}]
  (cond
    only   (->set only)
    except (complement (->set except))
    :else  (constantly true)))

(defmacro dbtest
  "Executes the body against all configured db strategies"
  [test-name & body]
  (let [mdata (meta test-name)
        strategies (filter (include-strategy? mdata)
                           (-> env :db :strategies))]
    `(do
       ~@(for [[strategy-name config] strategies]
           (let [q-test-name (with-meta
                               (symbol (str (name test-name)
                                            "-"
                                            (name strategy-name)))
                               (-> mdata
                                   (dissoc :only :except)
                                   (merge {:strategy strategy-name})))]
             `(deftest ~q-test-name
                (let [idx# (thread-db-index)
                      thread-config# (thread-specific-config
                                       ~config
                                       idx#)]
                  (with-db-lock idx#
                    (db/with-storage [thread-config#]
                      (db/reset (db/storage))
                      ~@body)))))))))
