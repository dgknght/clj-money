(ns clj-money.models.users-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
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
                                             :email
                                             :password])))
            expected [{:first-name "John"
                       :last-name "Doe"
                       :email "john@doe.com"}]]
        (is (= expected users))))
    (testing "It returns a user map"
      (is (number? (:id user)) "The id should be a number")
      (is (= {:first-name "John"
              :last-name "Doe"
              :email "john@doe.com"}
             (dissoc user :id))
          "The map should contain the user properties"))))

(defn assert-validation-error
  "Asserts the expected validation error"
  [trigger-fn attribute message]
  (let [result (trigger-fn)]
      (is (= [message]
             (validation/get-errors result attribute))
          (format "Expected error \"%s\" on %s, but it was not found." message attribute))))

(deftest try-to-create-with-invalid-data
  (testing "Email is required"
    (assert-validation-error #(users/create storage-spec
                                           (dissoc attributes :email))
                             :email
                             "Email is required"))
  (testing "Email is unique"
    (users/create storage-spec attributes)
    (assert-validation-error #(users/create storage-spec attributes)
                             :email
                             "Email is already taken"))
  (testing "Email must be a valid email address"
    (assert-validation-error #(users/create storage-spec
                                            (assoc attributes
                                                   :email "notavalidemail"))
                             :email
                             "Email is not valid"))
  (testing "First name is required"
    (assert-validation-error #(users/create storage-spec
                                            (dissoc attributes :first-name))
                             :first-name
                             "First name is required"))
  (testing "First name cannot be empty"
    (assert-validation-error #(users/create storage-spec
                                            (assoc attributes :first-name ""))
                             :first-name
                             "First name is required")
    (assert-validation-error #(users/create storage-spec
                                            (dissoc attributes :last-name))
                             :last-name
                             "Last name is required")) 
  (testing "Last name cannot be empty"
    (assert-validation-error #(users/create storage-spec
                                            (dissoc attributes :last-name))
                             :last-name
                             "Last name is required")))

(deftest authenticate-a-user
  (let [user (users/create storage-spec attributes)
        actual (users/authenticate storage-spec {:username "john@doe.com"
                                               :password "please01"})
        expected {:identity (:id user)
                  :id (:id user)
                  :email "john@doe.com"
                  :first-name "John"
                  :last-name "Doe"
                  :type :cemerick.friend/auth
                  :roles #{:user}}]
    (is (= expected actual) "The returned value should be the user information")))
