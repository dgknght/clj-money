(ns clj-money.models.identities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.model-helpers :as helpers :refer [assert-invalid]]
            [clj-money.models.identities :as idents]))

(use-fixtures :each reset-db)

(def ^:private create-context
  [(factory :user {:user/email "john@doe.com"})])

(defn- attributes []
  #:identity{:user (find-user "john@doe.com")
             :provider :google
             :provider-id "abc123"})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:identity/user]))

(deftest create-an-identity
  (with-context create-context
    (assert-created (attributes))))

(deftest provider-is-required
  (with-context create-context
    (assert-invalid (dissoc (attributes) :identity/provider)
                    {:identity/provider ["Provider is required"]})))

(deftest provider-id-is-required
  (with-context create-context
    (assert-invalid (dissoc (attributes) :identity/provider-id)
                    {:identity/provider-id ["Provider is required"]}))) ; TODO: Allow for this to be "Provider id is required"

(deftest user-is-required
  (with-context create-context
    (assert-invalid (dissoc (attributes) :identity/user)
                    {:identity/user ["User is required"]})))

(def find-context
  (conj create-context
        #:identity{:user "john@doe.com"
                   :provider :google
                   :provider-id "abc123"}))

(def profile
  {:given_name "John"
   :email "john@doe.com"
   :id "abc123"
   :name "John Doe"
   :family_name "Doe"})

(deftest find-a-user-from-profile-via-identity
  (with-context find-context

    (pprint {::find-or-create-from-profile (idents/find-or-create-from-profile
                                             :google
                                             profile)})

    (is (comparable? #:user{:email "john@doe.com"}
                     (idents/find-or-create-from-profile
                       :google
                       profile))
        "The correct user record is returned")))

; (deftest find-a-user-from-profile-and-create-identity
;   (let [ctx (realize create-context)
;         user (find-user ctx "john@doe.com")
;         retrieved (idents/find-or-create-from-profile
;                    :google
;                    profile)]
;     (is (comparable? user retrieved :id :email :first-name :last-name)
;         "The correct user record is returned")))
; 
; (deftest create-a-user-from-a-profile
;   (let [retrieved (idents/find-or-create-from-profile
;                     :google
;                     profile)]
;     (is (comparable? {:email "john@doe.com"
;                       :first-name "John"
;                       :last-name "Doe"}
;                      retrieved)
;         "The correct user record is returned")))
