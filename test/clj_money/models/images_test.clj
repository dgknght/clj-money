(ns clj-money.models.images-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest use-fixtures is]]
            [clojure.java.io :as io]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.io :refer [read-bytes]]
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
          :body (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))})

(defn- assert-created
  [attr]
  (helpers/assert-created attr {:refs [:image/user]
                                :ignore-attributes [:image/body]
                                :compare-result? false}))

(deftest create-an-image
  (with-context image-context
    (assert-created (attributes))))

; (deftest user-id-is-required
;   (let [context (realize image-context)
;         result (images/create (dissoc (attributes context) :user-id))]
;     (is (invalid? result [:user-id] "User is required"))))
; 
; (deftest original-filename-is-required
;   (let [context (realize image-context)
;         result (images/create (dissoc (attributes context) :original-filename))]
;     (is (invalid? result [:original-filename] "Original filename is required"))))
; 
; (deftest body-hash-is-generated
;   (let [context (realize image-context)
;         result (images/create (attributes context))]
;     (is (valid? result))
;     (is (= "7e3feff7f2dc501a32af044d3ead2f8667649b79"
;            (:body-hash result))
;         "The body-hash value is calculated and saved")))
; 
; (deftest body-hash-is-unique-for-each-user
;   (let [context (realize image-context)
;         _ (images/create (attributes context))
;         image-2 (images/create (attributes context))]
;     (is (invalid? image-2 [:body-hash] "The image has already been added"))))
; 
; (deftest body-is-required
;   (let [context (realize image-context)
;         result (images/create (dissoc (attributes context) :body))]
;     (is (invalid? result [:body] "Body is required"))))
; 
; (deftest content-type-is-required
;   (let [context (realize image-context)
;         result (images/create (dissoc (attributes context)
;                                       :content-type))]
;     (is (invalid? result [:content-type] "Content type is required"))))
