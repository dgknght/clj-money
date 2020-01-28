(ns clj-money.models.users-test
  (:require [clojure.test :refer [deftest testing use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.models.users :as users]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error
                                            selective=]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def attributes {:first-name "John"
                 :last-name "Doe"
                 :email "john@doe.com"
                 :password "please01"})

(deftest create-a-user
  (let [user (users/create storage-spec attributes)]
    (testing "An created user can be retreived"
      (let [users (->> (users/select storage-spec)
                       (map #(select-keys % [:first-name
                                             :last-name
                                             :email])))
            expected [{:first-name "John"
                       :last-name "Doe"
                       :email "john@doe.com"}]]
        (is (= expected users))))
    (testing "It returns a user map"
      (is (number? (:id user)) "The id should be a number")
      (is (= {:first-name "John"
              :last-name "Doe"
              :email "john@doe.com"}
             (select-keys user [:first-name :last-name :email]))
          "The map should contain the user properties"))))

(deftest try-to-create-with-duplicate-email
  (testing "Email is unique"
    (users/create storage-spec attributes)
    (assert-validation-error
      :email
      "Email is already taken"
      (users/create storage-spec attributes))))

(deftest try-to-create-with-invalid-data
  (testing "Email is required"
    (assert-validation-error
      :email
      "Email is required"
      (users/create storage-spec (dissoc attributes :email))))
  (testing "Email must be a valid email address"
    (assert-validation-error
      :email
      "Email must be a valid email"
      (users/create storage-spec (assoc attributes :email "notavalidemail"))))
  (testing "First name is required"
    (assert-validation-error
      :first-name
      "First name is required"
      (users/create storage-spec (dissoc attributes :first-name))))
  (testing "First name cannot be empty"
    (assert-validation-error
      :first-name
      "First name cannot be empty"
      (users/create storage-spec (assoc attributes :first-name "")))
    (assert-validation-error
      :last-name
      "Last name is required"
      (users/create storage-spec (dissoc attributes :last-name))))
  (testing "Last name cannot be empty"
    (assert-validation-error
      :last-name
      "Last name cannot be empty"
      (users/create storage-spec (assoc attributes :last-name "")))))

(deftest authenticate-a-user
  #_(let [user (users/create storage-spec attributes)
        actual (dissoc (users/authenticate storage-spec
                                           {:username "john@doe.com"
                                            :password "please01"})
                       :updated-at
                       :created-at)
        expected {:identity (:id user)
                  :id (:id user)
                  :email "john@doe.com"
                  :first-name "John"
                  :last-name "Doe"
                  :roles #{:user}}]
    (if-not (= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))
    (is (= expected actual) "The returned value should be the user information")))

(deftest set-a-password-reset-token
  (let [user (users/create storage-spec attributes)
        token (users/create-password-reset-token storage-spec user)
        retrieved (users/find-by-token storage-spec token)]
    (is (re-matches #"^[a-z0-9]{32}$" token)
        "A valid tokenis returned")
    (is (= (:id user) (:id retrieved))
        "The user can be retrieved using the token")))

(deftest cannot-retrieve-a-user-with-an-expired-token
  (let [user (users/create storage-spec attributes)
        token (t/do-at (t/date-time 2017 3 2 12 0 0)
                (users/create-password-reset-token storage-spec user))
        retrieved (t/do-at (t/date-time 2017 3 3 12 0 0)
                    (users/find-by-token storage-spec token))]
    (is (nil? retrieved)
        "The user is not returned if the token has expired")))

(deftest reset-a-password
  (let [user (users/create storage-spec attributes)
        token (users/create-password-reset-token storage-spec user)
        _ (users/reset-password storage-spec token "newpassword")
        new-auth (users/authenticate storage-spec {:username "john@doe.com"
                                                   :password "newpassword"})
        old-auth (users/authenticate storage-spec {:username "john@doe.com"
                                                   :password "please01"})
        retrieved (users/find-by-token storage-spec token)]
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
    (let [user (users/create storage-spec attributes)
          result (users/find-or-create-from-profile storage-spec profile)]
      ; TODO: assert that the identity record is created

      (is (selective= user result :first-name :last-name :email :id)
          "The existing user is returned"))))
