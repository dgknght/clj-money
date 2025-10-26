(ns clj-money.entities.images-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [is]]
            [clojure.java.io :as io]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.images.sql]
            [clj-money.io :refer [read-bytes]]
            [clj-money.entities.images :as imgs]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.test-helpers :refer [dbtest]]
            [clj-money.entity-helpers :refer [assert-invalid] :as helpers]))

(def image-context
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "Please001!"}])

(defn attributes []
  #:image{:user (find-user "john@doe.com")
          :original-filename "sample.gnucash"
          :content-type "application/gnucash"
          :uuid "9ce09a549c89ad44100e944410fb3b1b84dc0bea"})

(defn- assert-created
  [attr]
  (helpers/assert-created attr
                          :refs [:image/user]
                          :ignore-attributes [:image/body]
                          :compare-result? false))

(dbtest create-an-image
  (with-context image-context
    (assert-created (attributes))))

(dbtest user-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/user)
                    {:image/user ["User is required"]})))

(dbtest original-filename-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/original-filename)
                    {:image/original-filename ["Original filename is required"]})))

(def ^:private existing-image-context
  (conj image-context
        #:image{:user "john@doe.com"
                :original-filename "sample.gnucash"
                :content-type "application/gnucash"
                :content (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))}))

(dbtest uuid-is-unique-for-each-user
  (with-context existing-image-context
    (assert-invalid (attributes)
                    {:image/uuid ["The image has already been added"]})))

(dbtest content-type-is-required
  (with-context image-context
    (assert-invalid (dissoc (attributes) :image/content-type)
                    {:image/content-type ["Content type is required"]})))

(dbtest find-or-create-an-image
  (with-context image-context
    (let [image (imgs/find-or-create (-> (attributes)
                                         (dissoc :image/uuid)
                                         (assoc :image/content
                                                (-> "resources/fixtures/sample.gnucash"
                                                    io/input-stream
                                                    read-bytes))))]
      (is (comparable? {:image/uuid "9ce09a549c89ad44100e944410fb3b1b84dc0bea"}
                       image)
          "A UUID is created and added to the image"))))
