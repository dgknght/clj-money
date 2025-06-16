(ns clj-money.models.users-test
  (:require [clojure.test :refer [testing is]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.test-assertions]
            [clj-money.model-helpers :refer [assert-created
                                             assert-updated
                                             assert-invalid
                                             assert-deleted]]
            [clj-money.models :as models]
            [clj-money.db.sql.ref]
            [clj-money.db.datomic.ref]
            [clj-money.dates :refer [with-fixed-time]]
            [clj-money.models.users :as users]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.test-helpers :refer [dbtest]]))

(def attributes #:user{:first-name "John"
                       :last-name "Doe"
                       :email "john@doe.com"
                       :password "please01"})

(dbtest create-a-user
  (let [user (assert-created attributes
                             :ignore-attributes [:user/password])]
    (is (not (:user/password user))
        "The password is not returned")
    (is (not (:user/password-reset-token user))
        "The password reset token is not returned")
    (is (not (:user/token-expires-at user))
        "The token expiration is not returned")))

(dbtest first-name-is-required
  (assert-invalid (dissoc attributes :user/first-name)
                  {:user/first-name ["First name is required"]}))

(dbtest first-name-cannot-be-empty
  (assert-invalid (assoc attributes :user/first-name "")
                  {:user/first-name ["First name is required"]}))

(dbtest last-name-is-required
  (assert-invalid (dissoc attributes :user/last-name)
                  {:user/last-name ["Last name is required"]}))

(dbtest last-name-cannot-be-empty
  (assert-invalid (assoc attributes :user/last-name "")
                  {:user/last-name ["Last name is required"]}))

(dbtest email-is-required
  (assert-invalid (dissoc attributes :user/email)
                  {:user/email ["Email is required"]}))

(dbtest email-cannot-be-empty
  (assert-invalid (assoc attributes :user/email "")
                  {:user/email ["Email is required"]}))

(def ^:private existing-user-ctx
  [#:user{:first-name "John"
          :last-name "Doe"
          :email "john@doe.com"
          :password "please01"}])

(dbtest email-is-unique
  (with-context existing-user-ctx
    (assert-invalid attributes
                    {:user/email ["Email is already in use"]})))

(dbtest email-must-be-well-formed
  (assert-invalid (assoc attributes :user/email "notvalid")
                  {:user/email ["Email must be a valid email address"]}))

(dbtest authenticate-a-user
  (with-context existing-user-ctx
    (let [user (find-user "john@doe.com")
          expected {:identity (:id user)
                    :roles #{:user}
                    :user/email "john@doe.com"
                    :user/first-name "John"
                    :user/last-name "Doe"}
          authenticated (users/authenticate {:username "john@doe.com"
                                            :password "please01"})]
      (is (comparable? expected authenticated)
          "The returned value should be the user information")
      (is (nil? (:user/password authenticated))
          "The password is excluded from the return value."))))

(dbtest set-a-password-reset-token
  (with-context existing-user-ctx
    (let [user (find-user "john@doe.com")
          token (users/create-password-reset-token user)
          retrieved (users/find-by-token token)]
      (is (re-matches #"^[a-z0-9]{32}$" token)
          "A valid tokenis returned")
      (is (= (:id user) (:id retrieved))
          "The user can be retrieved using the token"))))

(dbtest cannot-retrieve-a-user-with-an-expired-token
  (with-context existing-user-ctx
    (let [user (find-user "john@doe.com")
          token (with-fixed-time "2017-03-02T12:00:00Z"
                  (users/create-password-reset-token user))
          retrieved (with-fixed-time "2017-03-03T12:00:00Z"
                      (users/find-by-token token))]
      (is (nil? retrieved)
          "The user is not returned if the token has expired"))))

(dbtest reset-a-password
  (with-context existing-user-ctx
    (let [user (find-user "john@doe.com")
          token (users/create-password-reset-token user)]
      (users/reset-password token "newpassword")
      (is (users/authenticate {:username "john@doe.com"
                               :password "newpassword"})
          "The user can be authenticated with the new password")
      (is (nil? (users/authenticate {:username "john@doe.com"
                                     :password "please01"}))
          "The user cannot be authenticated with the old password")
      (is (nil? (users/find-by-token token))
          "The user cannot be retrieved with a token that has been used"))))

(def ^:private profile
  {:given_name "John"
   :email "john@doe.com"
   :id "123"
   :name "John Doe"
   :family_name "Doe"})

(dbtest find-or-create-a-user-by-oauth-profile
  (testing "an existing user without identity"
    (let [user (models/put attributes)
          result (users/find-or-create-from-profile profile)]
      ; TODO: assert that the identity record is created

      (is (comparable? user result :id :first-name :last-name :email)
          "The existing user is returned"))))

(dbtest update-a-user
  (with-context existing-user-ctx
    (assert-updated (find-user "john@doe.com")
                    {:user/first-name "J-man"})))

(dbtest delete-a-user
  (with-context existing-user-ctx
    (assert-deleted (find-user "john@doe.com"))))
