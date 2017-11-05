(ns clj-money.models.users-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.test-helpers :refer :all]))

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
      "Email is not valid"
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
  (let [user (users/create storage-spec attributes)
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
                  :type :cemerick.friend/auth
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
        token (with-time (t/date-time 2017 3 2 12 0 0)
                (users/create-password-reset-token storage-spec user))
        retrieved (with-time (t/date-time 2017 3 3 12 0 0)
                    (users/find-by-token storage-spec token))]
    (is (nil? retrieved)
        "The user is not returned if the token has expired")))

; a user cannot be retrieved by token after the token has been used
