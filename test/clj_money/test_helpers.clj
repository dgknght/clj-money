(ns clj-money.test-helpers
  (:require [clojure.test :refer [is]]
            [clojure.java.jdbc :as jdbc]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.validation :as validation]))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (jdbc/with-db-connection [db (env :db)]
    (jdbc/execute! db "truncate table users cascade"))
  (f))

(defn subset?
  "Accepts two maps, the first of which may or may not
  be a subset of the second. Returns true if the first
  is a subset of the second, false if not"
  [smaller-map larger-map]
  (= (select-keys larger-map (keys smaller-map))
     smaller-map))

(defmacro assert-validation-error
  "Asserts the expected validation error"
  [attribute message & body]
  `(let [result# ~@body
         found-message# (validation/error-messages result# ~attribute)]
     (is (= [~message] found-message#)
         (format "Expected error \"%s\" on %s, but received %s instead"
                 ~message
                 ~attribute
                 found-message#))))

(defmacro assert-throws-ex-info
  "Tests to see if the specified code raises ExceptionInfo
  containing the specified data"
  [expected-data & body]
  `(try
     ~@body
     (is false "An exception was expected, but none was thrown.")
     (catch clojure.lang.ExceptionInfo e#
       (is (subset? ~expected-data (ex-data e#))
           (str "Expected ex-data to contain " ~expected-data)))))

(defmacro assert-throws-ex-info-with-key
  "Tests to see if the specified code raises ExceptionInfo
  containing a non-nil value at the specified key. The expected
  key can be a sequence for nested data structures."
  [expected-key & body]
  `(try
     ~@body
     (is false "An exception was expected, but none was thrown.")
     (catch clojure.lang.ExceptionInfo e#
       (let [data# (ex-data e#)]
         (is (not (nil? (if (seq ~expected-key)
                          (get-in data# ~expected-key)
                          (get data# ~expected-key))))
             (str "Expected a value at the key "
                  ~expected-key
                  ", but none was found in "
                  data#))))))

(defmacro assert-throws-validation-exception
  "Tests to see if the specified code raises a validation
  exception with the specified violation errors"
  [validation-errors & body]
  `(assert-throws-ex-info {:error ~validation-errors} ~@body))

(defn- simplify-accounts
  [accounts additional-attributes]
  (map #(if (seq (:children %))
          (-> %
              (select-keys (concat [:name :path :children] additional-attributes))
              (update-in [:children] simplify-accounts additional-attributes))
          (select-keys % [:name :path]))
       accounts))

(defn simplify-account-groups
  "Accept a list of hashes containing :type keyword and :accounts [],
  drill down into each account, filtering out every attribute of the
  account except the name"
  ([groups]
   (simplify-account-groups groups []))
  ([groups additional-attributes]
   (map #(update-in % [:accounts] (fn [accounts]
                                    (simplify-accounts accounts additional-attributes)))
        groups)))

(defn context-errors
  [context]
  (reduce (fn [result [category models]]
            (let [invalid-models (filter #(validation/has-error? %) models)]
              (if (seq invalid-models)
                (assoc result category invalid-models)
                result)))
          {}
          context))

(defmacro with-authentication
  [_user & body]
  `(do ~@body))

(defmacro with-time
  [at-time & body]
  `(with-redefs [t/now (fn [] ~at-time)]
     ~@body))

(defn selective=
  [expected actual & attributes]
  {:pre [expected (map? expected) (if actual (map? actual) true)]}
  (let [attr (if (seq attributes)
               attributes
               (keys expected))
        e (select-keys expected attr)
        a (select-keys actual attr)]
    (= e a)))

(defn seq-containing?
  [expected actual & attributes]
  (when (= (count expected) (count actual))
    (->> actual
         (zipmap expected)
         (every? (fn [[e a]] (apply selective= e a attributes))))))

(defmacro assert-contains
  [expected actual msg]
  `(is (= ~expected
          (select-keys ~actual (keys ~expected)))
       ~msg))

(defn- no-prune?
  [model]
  (-> model meta :no-prune))

(defmulti ^:private prune-to
  (fn [target model]
    (cond
      (no-prune? model) :default
      (map? target) :map
      (sequential? target) :seq)))

(defmethod prune-to :map
  [target model]
  (->> (select-keys target (keys model))
       (map (fn [entry]
              (update-in entry
                         [1]
                         prune-to
                         (get-in model (take 1 entry)))))
       (into {})))

(defmethod prune-to :seq
  [target model]
  (map-indexed #(prune-to %2 (nth model %1))
       target))

(defmethod prune-to :default
  [target _]
  target)

(defn assert-comparable
  [expected actual msg]
  (is (= expected
         (prune-to actual expected))
      msg))
