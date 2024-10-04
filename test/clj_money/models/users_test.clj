(ns clj-money.models.users-test
  (:require [clojure.test :refer [deftest testing use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.db.sql]
            [clj-money.db.sql.ref]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.models.users :as users]
            [clj-money.test-context :refer [with-context
                                            find-user]]
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
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors {:user/first-name ["First name is required"]}}
        (users/put (dissoc attributes :user/first-name)))))

(deftest first-name-cannot-be-empty
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors {:user/first-name ["First name is required"]}}
        (users/put (assoc attributes :user/first-name "")))))

(deftest last-name-is-required
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors {:user/last-name ["Last name is required"]}}
        (users/put (dissoc attributes :user/last-name)))))

(deftest last-name-cannot-be-empty
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors {:user/last-name ["Last name is required"]}}
        (users/put (assoc attributes :user/last-name "")))))

(deftest email-is-required
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors {:user/email ["Email is required"]}}
        (users/put (dissoc attributes :user/email)))))

(deftest email-cannot-be-empty
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors {:user/email ["Email is required"]}}
        (users/put (assoc attributes :user/email "")))))

(def ^:private existing-user-ctx
  {:users [#:user{:first-name "John"
                  :last-name "Doe"
                  :email "john@doe.com"
                  :password "please01"}]})

(deftest email-is-unique
  (with-context existing-user-ctx
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:user/email ["Email is already in use"]}}
          (users/put attributes)))))

(deftest email-must-be-well-formed
  (is (thrown-with-ex-data?
        "Validation failed"
        {::v/errors {:user/email ["Email must be a valid email address"]}}
        (users/put (assoc attributes :user/email "notvalid")))))

(deftest authenticate-a-user
  (with-context existing-user-ctx
    (let [user (find-user "john@doe.com")
          expected {:identity (:id user)
                    :roles #{:user}
                    :user/email "john@doe.com"
                    :user/first-name "John"
                    :user/last-name "Doe"}]
      (is (comparable? expected
                       (users/authenticate {:username "john@doe.com"
                                            :password "please01"}))
          "The returned value should be the user information"))))

(deftest set-a-password-reset-token
  (with-context existing-user-ctx
    (let [user (find-user "john@doe.com")
          token (users/create-password-reset-token user)
          retrieved (users/find-by-token token)]
      (is (re-matches #"^[a-z0-9]{32}$" token)
          "A valid tokenis returned")
      (is (= (:id user) (:id retrieved))
          "The user can be retrieved using the token"))))

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
