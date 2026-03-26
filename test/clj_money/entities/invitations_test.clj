(ns clj-money.entities.invitations-test
  (:require [clojure.test :refer [is]]
            [dgknght.app-lib.test-assertions]
            [clj-money.entity-helpers :refer [assert-created
                                              assert-invalid]]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private user-ctx
  [#:user{:first-name "Admin"
          :last-name "User"
          :email "admin@example.com"
          :password "please01"
          :roles #{:admin}}])

(defn- attributes []
  #:invitation{:recipient "new@example.com"
               :status :unsent
               :user (find-user "admin@example.com")})

(dbtest create-an-invitation
  (with-context user-ctx
    (let [inv (assert-created (attributes) :refs [:invitation/user])]
      (is (= :unsent (:invitation/status inv))
          "The status is a keyword"))))

(dbtest recipient-is-required
  (with-context user-ctx
    (assert-invalid (dissoc (attributes) :invitation/recipient)
                    {:invitation/recipient ["Recipient is required"]})))

(dbtest recipient-must-be-valid-email
  (with-context user-ctx
    (assert-invalid (assoc (attributes) :invitation/recipient "notanemail")
                    {:invitation/recipient ["Recipient must be a valid email address"]})))

(dbtest recipient-must-not-already-be-a-user
  (with-context user-ctx
    (assert-invalid (assoc (attributes) :invitation/recipient "admin@example.com")
                    {:invitation/recipient ["Recipient is already in use"]})))

(dbtest status-is-required
  (with-context user-ctx
    (assert-invalid (dissoc (attributes) :invitation/status)
                    {:invitation/status ["Status is required"]})))

(dbtest status-must-be-valid
  (with-context user-ctx
    (assert-invalid (assoc (attributes) :invitation/status :invalid)
                    {:invitation/status ["Status must be unsent, accepted, sent, or declined"]})))
