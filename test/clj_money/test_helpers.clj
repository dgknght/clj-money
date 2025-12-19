(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [clj-money.config :refer [env]]
            [dgknght.app-lib.test :as test]
            [clj-money.decimal :as d]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.entities :as entities]))

; TODO: Remove this an just use the reset in the dbtest so that we don't have to duplicate the strategy selection logic
(def active-db-config
  (get-in env [:db :strategies (get-in env [:db :active])]))

(def thread-indices (atom {:next 0
                           :saved {}}))

(defn- thread-index []
  (let [id (.getId (Thread/currentThread))]
    (or (get-in @thread-indices [:saved id])
        (get-in (swap! thread-indices
                       (fn [{:keys [next saved] :as a}]
                         (if (contains? saved id)
                           a
                           (-> a
                               (assoc-in [:saved id] next)
                               (update-in [:next] inc)))))
                [:saved id]))))

(defmulti thread-specific-config
  "Creates a thread-specific database configuration to allow parallel test execution"
  ::db/strategy)

(defmethod thread-specific-config ::db/datomic-peer
  [config]
  (update-in config
             [:uri]
             #(format "%s_%s"
                      %
                      (thread-index))))

(defmethod thread-specific-config ::db/sql
  [config]
  (update-in config
             [:dbname]
             #(format "%s_%s"
                      %
                      (thread-index))))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (let [config (thread-specific-config active-db-config)]
    (db/with-storage [config]
      (db/reset (db/storage))
      (f))))

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

(defn edn-body
  [req payload]
  (-> req
      (req/content-type "application/edn")
      (req/body (pr-str payload))))

(defn parse-edn-body
  [res]
  (test/parse-edn-body res :readers {'clj-money/local-date t/local-date
                                     'clj-money/local-date-time t/local-date-time
                                     'clj-money/decimal d/d}))

(def ^:dynamic *strategy* nil)

(defn ->set
  [v]
  (if (coll? v)
    (set v)
    #{v}))

(defn include-strategy
  [{:keys [only exclude]}]
  (cond
    only    (list 'clj-money.test-helpers/->set only)
    exclude `(complement ~(clj-money.test-helpers/->set exclude))
    :else   '(constantly true)))

(def isolate (when-let [isolate (env :isolate)]
               #{(if (string? isolate)
                   (keyword isolate)
                   isolate)}))

(def ignore-strategy (if isolate
                       (complement isolate)
                       (if-let [ignore (env :ignore-strategy)]
                         (->set ignore)
                         (constantly false))))

(def honor-strategy (complement ignore-strategy))

(defn extract-opts
  [args]
  (if (map? (first args))
    args
    (cons {} args)))

(defmacro dbtest
  [test-name & body-and-opts]
  (let [[opts & body] (extract-opts body-and-opts)]
    `(deftest ~test-name
       (doseq [[name# config#] (filter (comp (every-pred ~(include-strategy opts)
                                                         honor-strategy)
                                             first)
                                       (-> env :db :strategies))]
         (binding [*strategy* (keyword name#)]
           (testing (format "database strategy %s" name#)
             (let [thread-config# (thread-specific-config config#)]
               (db/with-storage [thread-config#]
                 (db/reset (db/storage))
                 ~@body))))))))
