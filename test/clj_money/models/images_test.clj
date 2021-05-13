(ns clj-money.models.images-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.java.io :as io]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.io :refer [read-bytes]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize]]
            [clj-money.test-helpers :refer [reset-db]]
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
    (is (valid? result))
    (is (:id result)
        "The result has an :id value")))

(deftest user-id-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context) :user-id))]
    (is (invalid? result [:user-id] "User is required"))))

(deftest original-filename-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context) :original-filename))]
    (is (invalid? result [:original-filename] "Original filename is required"))))

(deftest body-hash-is-generated
  (let [context (realize image-context)
        result (images/create (attributes context))]
    (is (valid? result))
    (is (= "7e3feff7f2dc501a32af044d3ead2f8667649b79"
           (:body-hash result))
        "The body-hash value is calculated and saved")))

(deftest body-hash-is-unique-for-each-user
  (let [context (realize image-context)
        _ (images/create (attributes context))
        image-2 (images/create (attributes context))]
    (is (invalid? image-2 [:body-hash] "The image has already been added"))))

(deftest body-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context) :body))]
    (is (invalid? result [:body] "Body is required"))))

(deftest content-type-is-required
  (let [context (realize image-context)
        result (images/create (dissoc (attributes context)
                                      :content-type))]
    (is (invalid? result [:content-type] "Content type is required"))))
