(ns clj-money.models.images-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.java.io :as io]
            [clj-factory.core :refer [factory]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.images :as images]))

(use-fixtures :each reset-db)

(def image-context
  {:users [(factory :user)]})

(defn attributes
  [context]
  {:user-id (-> context :users first :id)
   :original-filename "sample.gnucash"
   :content-type "application/gnucash"
   :body (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))})

(deftest create-an-image
  (let [context (realize image-context)
        result (images/create (attributes context))]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (:id result)
        "The result has an :id value")))

(deftest user-id-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context) :user-id))]
    (is (seq (validation/error-messages result :user-id))
        "The result has a validation error on :user-id")))

(deftest original-filename-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context) :original-filename))]
    (is (seq (validation/error-messages result :original-filename))
        "The result has a validation error on :original-filename")))

(deftest body-hash-is-generated
  (let [context (realize image-context)
        result (images/create (attributes context))]
    (is (empty? (validation/error-messages result))
        "There are no validation errors")
    (is (= "7e3feff7f2dc501a32af044d3ead2f8667649b79"
           (:body-hash result))
        "The body-hash value is calculated and saved")))

(deftest body-hash-is-unique-for-each-user
  (let [context (realize image-context)
        _ (images/create (attributes context))
        image-2 (images/create (attributes context))]
    (is (seq (validation/error-messages image-2 :body-hash))
        "The result has a validation error on :body")))

(deftest body-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context) :body))]
    (is (seq (validation/error-messages result :body))
        "The result has a validation error on :body")))

(deftest content-type-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context)
                                      :content-type))]
    (is (not (validation/valid? result))
        "The value can be retreived from the database")
    (is (seq (validation/error-messages result :content-type))
        "The content-type attribute has an error message")))
