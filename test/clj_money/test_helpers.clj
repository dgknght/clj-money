(ns clj-money.test-helpers
  (:require [clojure.test :refer [is]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.jdbc :as jdbc]
            [clj-time.core :as t]
            [clj-money.validation :as validation])
  (:import org.joda.time.LocalDate))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [db-spec f]
  (jdbc/with-db-connection [db db-spec]
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
          ( -> %
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

(defn- find-in-context
  [context model-group-key model-id-key model-id]
  (->> context
       model-group-key
       (filter #(= model-id (model-id-key %)))
       first))

(defn find-user
  [context email]
  (find-in-context context :users :email email))

(defn find-users
  [context & emails]
  (map #(find-user context %) emails))

(defn find-entity
  [context entity-name]
  (find-in-context context :entities :name entity-name))

(defn find-entities
  [context & entity-names]
  (map #(find-entity context %) entity-names))

(defn find-import
  [context entity-name]
  (find-in-context context :imports :entity-name entity-name))

(defn find-imports
  [context & entity-names]
  (map #(find-import context %) entity-names))

(defn find-grant
  [context entity-id user-id]
  (->> context
       :grants
       (filter #(and (= entity-id (:entity-id %))
                     (= user-id (:user-id %))))
       first))

(defn find-account
  [context account-name]
  (find-in-context context :accounts :name account-name))

(defn find-accounts
  [context & account-names]
  (map #(find-account context %) account-names))

(defn find-attachment
  [context caption]
  (find-in-context context :attachments :caption caption))

(defn find-image
  [context original-filename]
  (find-in-context context :images :original-filename original-filename))

(defn find-commodity
  [context symbol]
  (find-in-context context :commodities :symbol symbol))

(defn find-commodities
  [context & symbols]
  (map #(find-commodity context %) symbols))

(defn find-budget
  [context budget-name]
  (find-in-context context :budgets :name budget-name))

(defn find-price
  [context sym trade-date]
  (let [commodity (find-commodity context sym)]
    (->> context
         :prices
         (filter #(and (= (:id commodity) (:commodity-id %))
                       (= trade-date (:trade-date %))))
         first)))

(defn find-transaction
  [context transaction-date description]
  {:pre [(string? description) (instance? LocalDate transaction-date)]}

  (->> context
       :transactions
       (filter #(and (= transaction-date (:transaction-date %))
                     (= description (:description %))))
       first))

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

(defn pprint-diff
  [expected actual]
  (let [d (diff expected actual)]
    (when (or (first d)
              (second d))
      (pprint {:expected expected
               :actual actual
               :diff d}))))

(defn selective=
  [expected actual & attributes]
  (let [attr (if (seq attributes)
               attributes
               (keys expected))
        e (select-keys expected attr)
        a (select-keys actual attr)]
    (= e a)))
