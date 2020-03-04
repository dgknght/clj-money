(ns clj-money.models.images-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
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
   :content-type "application/gnucash"
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
    (is (seq (validation/error-messages result :user-id))
        "The result has a validation error on :user-id")))

(deftest original-filename-is-required
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (dissoc (attributes context) :original-filename))]
    (is (seq (validation/error-messages result :original-filename))
        "The result has a validation error on :original-filename")))

(deftest body-hash-is-generated
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (attributes context))]
    (is (empty? (validation/error-messages result))
        "There are no validation errors")
    (is (= "dd9c277e9bfdbdace10a06820e76b659a6b3a851"
           (:body-hash result))
        "The body-hash value is calculated and saved")))

(deftest body-hash-is-unique-for-each-user
  (let [context (serialization/realize storage-spec image-context)
        _ (images/create storage-spec (attributes context))
        image-2 (images/create storage-spec (attributes context))]
    (is (seq (validation/error-messages image-2 :body-hash))
        "The result has a validation error on :body")))

(deftest body-is-required
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (dissoc (attributes context) :body))]
    (is (seq (validation/error-messages result :body))
        "The result has a validation error on :body")))

(deftest content-type-is-required
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (dissoc (attributes context)
                                                        :content-type))]
    (is (not (validation/valid? result))
        "The value can be retreived from the database")
    (is (seq (validation/error-messages result :content-type))
        "The content-type attribute has an error message")))
