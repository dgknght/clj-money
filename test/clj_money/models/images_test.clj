(ns clj-money.models.images-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest use-fixtures is]]
            [clojure.java.io :as io]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models :as models]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.model-helpers :refer [assert-invalid] :as helpers]))

(use-fixtures :each reset-db)

(def image-context
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "Please001!"}])

(defn attributes []
  #:image{:user (find-user "john@doe.com")
          :original-filename "sample.gnucash"
          :content-type "application/gnucash"
          :body (-> "resources/fixtures/sample.gnucash"
                    io/input-stream
                    read-bytes)})

(defn- assert-created
  [attr]
  (helpers/assert-created attr
                          :refs [:image/user]
                          :ignore-attributes [:image/body]
                          :compare-result? false))

(deftest create-an-image
  (with-context image-context
    (assert-created (attributes))))

(deftest user-id-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/user)
                    {:image/user ["User is required"]})))

(deftest original-filename-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/original-filename)
                    {:image/original-filename ["Original filename is required"]})))

(deftest body-hash-is-generated
  (with-context image-context
    (let [result (models/put (attributes))]
      (is (comparable? {:image/body-hash "7e3feff7f2dc501a32af044d3ead2f8667649b79"}
                       result)
          "The body-hash value is calculated and saved"))))

(def ^:private existing-image-context
  (conj image-context
        #:image{:user "john@doe.com"
                :original-filename "sample.gnucash"
                :content-type "application/gnucash"
                :body (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))}))

(deftest body-hash-is-unique-for-each-user
  (with-context existing-image-context
    (assert-invalid (attributes)
                    {:image/body-hash ["The image has already been added"]})))

(deftest body-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/body)
                    {:image/body ["Body is required"]
                     :image/body-hash ["Body hash must be a string"]})))

(deftest content-type-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/content-type)
                    {:image/content-type ["Content type is required"]})))
