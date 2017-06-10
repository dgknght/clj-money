(ns clj-money.models.images-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.images :as images]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def image-context
  {:users [(factory :user)]})

(defn attributes
  [context]
  {:user-id (-> context :users first :id)
   :original-filename "sample.gnucash"
   :body-hash "dd9c277e9bfdbdace10a06820e76b659a6b3a851"
   :body (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))})

(deftest create-an-image
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (attributes context))]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (:id result)
        "The result has an :id value")))

(deftest user-id-is-required
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (dissoc (attributes context) :user-id))]
    (is (not (empty? (validation/error-messages result :user-id)))
        "The result has a validation error on :user-id")))

(deftest original-filename-is-required
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (dissoc (attributes context) :original-filename))]
    (is (not (empty? (validation/error-messages result :original-filename)))
        "The result has a validation error on :original-filename")))

(deftest body-hash-is-required
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (dissoc (attributes context) :body-hash))]
    (is (not (empty? (validation/error-messages result :body-hash)))
        "The result has a validation error on :body-hash")))

(deftest body-hash-is-unique-for-each-user
  (let [context (serialization/realize storage-spec image-context)
        image-1 (images/create storage-spec (attributes context))
        image-2 (images/create storage-spec (attributes context))]
    (is (not (empty? (validation/error-messages image-2 :body-hash)))
        "The result has a validation error on :body-hash")))

(deftest body-is-required
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (dissoc (attributes context) :body))]
    (is (not (empty? (validation/error-messages result :body)))
        "The result has a validation error on :body")))
