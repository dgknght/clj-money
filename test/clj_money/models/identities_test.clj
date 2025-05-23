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
    (is (comparable? #:user{:email "john@doe.com"}
                     (idents/find-or-create-from-profile
                       [:google
                        profile]))
        "The user record associated with the identity is returned")))

(deftest find-a-user-from-profile-and-create-identity
  (with-context create-context
    (is (comparable? #:user{:email "john@doe.com"}
                     (idents/find-or-create-from-profile
                       [:google
                       profile]))
        "The user record having the profile email is returned")))

(deftest create-a-user-from-a-profile
  (is (comparable? #:user{:email "john@doe.com"
                          :first-name "John"
                          :last-name "Doe"}
                   (idents/find-or-create-from-profile
                     [:google
                      profile]))
      "A new user record populated from the profile is returned"))
