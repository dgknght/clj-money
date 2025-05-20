(ns clj-money.models.images-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest use-fixtures is]]
            [clojure.java.io :as io]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.images.sql]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models.images :as imgs]
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
          :uuid "7e3feff7f2dc501a32af044d3ead2f8667649b79"})

(defn- assert-created
  [attr]
  (helpers/assert-created attr
                          :refs [:image/user]
                          :ignore-attributes [:image/body]
                          :compare-result? false))

(deftest create-an-image
  (with-context image-context
    (assert-created (attributes))))

(deftest user-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/user)
                    {:image/user ["User is required"]})))

(deftest original-filename-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/original-filename)
                    {:image/original-filename ["Original filename is required"]})))

(def ^:private existing-image-context
  (conj image-context
        #:image{:user "john@doe.com"
                :original-filename "sample.gnucash"
                :content-type "application/gnucash"
                :content (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))}))

(deftest uuid-is-unique-for-each-user
  (with-context existing-image-context
    (assert-invalid (attributes)
                    {:image/uuid ["The image has already been added"]})))

(deftest content-type-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/content-type)
                    {:image/content-type ["Content type is required"]})))

(deftest find-or-create-an-image
  (with-context image-context
    (let [image (imgs/find-or-create (-> (attributes)
                                         (dissoc :image/uuid)
                                         (assoc :image/content
                                                (-> "resources/fixtures/sample.gnucash"
                                                    io/input-stream
                                                    read-bytes))))]
      (is (comparable? {:image/uuid "7e3feff7f2dc501a32af044d3ead2f8667649b79"}
                       image)
          "A UUID is created an added to the image"))))
