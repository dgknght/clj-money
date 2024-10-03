(ns clj-money.models.users-test
  (:require [clojure.test :refer [deftest testing use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.test-assertions]
            [clj-money.db.sql]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.models.users :as users]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def attributes #:user{:first-name "John"
                       :last-name "Doe"
                       :email "john@doe.com"
                       :password "please01"})

(deftest create-a-user
  (let [user (users/put attributes)
        expected (dissoc attributes :user/password)]
    (is (valid? user))
    (is (:id user) "The result has an :id attributes")
    (is (not (:password user))
        "The password is not returned")
    (is (not (:password-reset-token user))
        "The password reset token is not returned")
    (is (not (:token-expires-at user))
        "The token expiration is not returned")
    (is (comparable? expected user)
        "The result contains the specified attribute values")
    (is (comparable? expected (users/find user))
        "The user can be retrieved from the data store.")))

(deftest first-name-is-required
  (is (invalid? (users/put (dissoc attributes :first-name))
                [:first-name]
                "First name is required")))

(deftest first-name-cannot-be-empty
  (is (invalid? (users/put (assoc attributes :first-name ""))
                [:first-name]
                "First name is required")))

(deftest last-name-is-required
  (is (invalid? (users/put (dissoc attributes :last-name))
                [:last-name]
                "Last name is required")))

(deftest last-name-cannot-be-empty
  (is (invalid? (users/put (assoc attributes :last-name ""))
                [:last-name]
                "Last name is required")))

(deftest email-is-required
  (is (invalid? (users/put (dissoc attributes :email))
                [:email]
                "Email is required")))

(deftest email-cannot-be-empty
  (is (invalid? (users/put (assoc attributes :email ""))
                [:email]
                "Email is required")))

(deftest email-is-unique
  (users/put attributes)
  (is (invalid? (users/put attributes)
                [:email]
                "Email is already in use")))

(deftest email-must-be-well-formed
  (is (invalid? (users/put (assoc attributes :email "notvalid"))
                [:email]
                "Email must be a valid email address")))

(deftest authenticate-a-user
  (let [user (users/put attributes)
        expected {:identity (:id user)
                  :id (:id user)
                  :email "john@doe.com"
                  :first-name "John"
                  :last-name "Doe"
                  :roles #{:user}}
        actual (select-keys (users/authenticate {:username "john@doe.com"
                                                 :password "please01"})

                            (keys expected))]
    (is (= expected actual) "The returned value should be the user information")))

(deftest set-a-password-reset-token
  (let [user (users/put attributes)
        token (users/create-password-reset-token user)
        retrieved (users/find-by-token token)]
    (is (re-matches #"^[a-z0-9]{32}$" token)
        "A valid tokenis returned")
    (is (= (:id user) (:id retrieved))
        "The user can be retrieved using the token")))

(deftest cannot-retrieve-a-user-with-an-expired-token
  (let [user (users/put attributes)
        token (with-fixed-time "2017-03-02T12:00:00Z"
                       (users/create-password-reset-token user))
        retrieved (with-fixed-time "2017-03-03T12:00:00Z"
                           (users/find-by-token token))]
    (is (nil? retrieved)
        "The user is not returned if the token has expired")))

(deftest reset-a-password
  (let [user (users/put  attributes)
        token (users/create-password-reset-token user)
        _ (users/reset-password token "newpassword")
        new-auth (users/authenticate {:username "john@doe.com"
                                      :password "newpassword"})
        old-auth (users/authenticate {:username "john@doe.com"
                                      :password "please01"})
        retrieved (users/find-by-token token)]
    (is new-auth "The user can be authenticated with the new password")
    (is (nil? old-auth) "The user cannot be authenticated with the old password")
    (is (nil? retrieved) "The user cannot be retrieved with a token that has been used")))

(def ^:private profile
  {:given_name "John"
   :email "john@doe.com"
   :id "123"
   :name "John Doe"
   :family_name "Doe"})

(deftest find-or-create-a-user-by-oauth-profile
  (testing "an existing user without identity"
    (let [user (users/put attributes)
          result (users/find-or-create-from-profile profile)]
      ; TODO: assert that the identity record is created

      (is (comparable? user result :id :first-name :last-name :email)
          "The existing user is returned"))))
