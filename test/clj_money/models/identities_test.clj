(ns clj-money.models.identities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user]]
            [clj-money.validation :as v]
            [clj-money.test-helpers :refer [reset-db
                                            selective=]]
            [clj-money.models.identities :as idents]))

(use-fixtures :each reset-db)

(def ^:private create-context
  {:users [(factory :user {:email "john@doe.com"})]})

(defn- attr
  [ctx]
  {:user-id (-> ctx (find-user "john@doe.com") :id)
   :provider :google
   :provider-id "abc123"})

(deftest create-an-identity
  (let [ctx (realize create-context)
        user (find-user ctx "john@doe.com")
        ident (idents/create (attr ctx))]
    (is ident "A value is returned")
    (is (:id ident) "An :id value is assigned")
    (is (= (:id user)  (:user-id ident)) "The user-id is retained")))

(deftest provider-is-required
  (let [ctx (realize create-context)
        ident (idents/create (dissoc (attr ctx) :provider))]
    (is ident "A value is returned")
    (is (v/has-error? ident :provider) "The model is returned with an error message")))

(deftest provider-id-is-required
  (let [ctx (realize create-context)
        ident (idents/create (dissoc (attr ctx) :provider-id))]
    (is ident "A value is returned")
    (is (v/has-error? ident :provider-id) "The model is returned with an error message")))

(deftest user-id-is-required
  (let [ctx (realize create-context)
        ident (idents/create (dissoc (attr ctx) :user-id))]
    (is ident "A value is returned")
    (is (v/has-error? ident :user-id) "The model is returned with an error message")))

(def find-context
  (assoc create-context
         :identities [{:user-id "john@doe.com"
                       :provider :google
                       :provider-id "abc123"}]))

(def profile
  {:given_name "John"
   :email "john@doe.com"
   :id "abc123"
   :name "John Doe"
   :family_name "Doe"})

(deftest find-a-user-from-profile-via-identity
  (let [ctx (realize find-context)
        user (find-user ctx "john@doe.com")
        retrieved (idents/find-or-create-from-profile
                    :google
                    profile)]
    (is (selective= user retrieved :id :email :first-name :last-name)
        "The correct user record is returned")))

(deftest find-a-user-from-profile-and-create-identity
  (let [ctx (realize create-context)
        user (find-user ctx "john@doe.com")
        retrieved (idents/find-or-create-from-profile
                    :google
                    profile)]
    (is (selective= user retrieved :id :email :first-name :last-name)
        "The correct user record is returned")))

(deftest create-a-user-from-a-profile
  (let [retrieved (idents/find-or-create-from-profile
                    :google
                    profile)]
    (is (selective= {:email "john@doe.com"
                     :first-name "John"
                     :last-name "Doe"}
                    retrieved)
        "The correct user record is returned")))
