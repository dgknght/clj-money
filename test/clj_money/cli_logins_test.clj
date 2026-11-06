(ns clj-money.cli-logins-test
  (:require [clojure.test :refer [is]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.db :refer [unserialize-id]]
            [clj-money.web.auth :refer [read-token]]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.test-helpers :refer [dbtest]]
            [clj-money.cli-logins :as cli-logins]))

(def ^:private secret "the-secret")

(def ^:private user-ctx
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "please01"}
   #:user{:email "jane@doe.com"
          :first-name "Jane"
          :last-name "Doe"
          :password "please01"}])

(defn- find-login
  [user-code]
  (entities/find-by {:cli-login/user-code user-code}))

(dbtest start-a-login
  (t/with-clock (t/fixed-clock "2026-11-05T12:00:00Z" "UTC")
    (let [{:cli-login/keys [device-code user-code]} (cli-logins/start)]
      (is (re-matches #"[0-9a-f]{64}" device-code)
          "The device code is a random secret")
      (is (re-matches #"[A-Z]{4}-[A-Z]{4}" user-code)
          "The user code is short, for the user to compare")
      (is (comparable? {:cli-login/device-code device-code}
                       (find-login user-code))
          "The login is saved")
      (is (= (t/instant "2026-11-05T12:10:00Z")
             (t/instant (:cli-login/expires-at (find-login user-code))))
          "The login expires in 10 minutes")
      (is (nil? (:cli-login/user (find-login user-code)))
          "The login is not yet approved"))))

(def ^:private pending-ctx
  (conj user-ctx
        #:cli-login{:device-code "the-device-code-0123456789abcdef"
                    :user-code "BCDF-GHJK"
                    :expires-at (t/plus (t/instant) (t/minutes 10))}))

(dbtest approve-a-login
  (with-context pending-ctx
    (let [user (find-user "john@doe.com")]
      (is (cli-logins/approve " bcdf-ghjk " user)
          "The code is trimmed and is not case-sensitive")
      (is (= (:id user)
             (get-in (find-login "BCDF-GHJK") [:cli-login/user :id]))
          "The login is linked to the approving user"))))

(dbtest an-unknown-code-cannot-be-approved
  (with-context pending-ctx
    (is (nil? (cli-logins/approve "ZZZZ-ZZZZ" (find-user "john@doe.com"))))
    (is (nil? (cli-logins/approve nil (find-user "john@doe.com"))))))

(dbtest a-pending-login-is-not-redeemed
  (with-context pending-ctx
    (is (= {:error "authorization_pending"}
           (cli-logins/redeem "the-device-code-0123456789abcdef" secret)))
    (is (find-login "BCDF-GHJK")
        "The login is kept")))

(dbtest an-unknown-device-code-is-not-redeemed
  (with-context pending-ctx
    (is (= {:error "invalid_grant"}
           (cli-logins/redeem "not-the-device-code" secret)))
    (is (= {:error "invalid_grant"}
           (cli-logins/redeem nil secret)))))

(def ^:private approved-ctx
  (conj user-ctx
        #:cli-login{:device-code "the-device-code-0123456789abcdef"
                    :user-code "BCDF-GHJK"
                    :user "john@doe.com"
                    :expires-at (t/plus (t/instant) (t/minutes 10))}))

(dbtest an-approved-login-cannot-be-approved-again
  (with-context approved-ctx
    (is (nil? (cli-logins/approve "BCDF-GHJK" (find-user "jane@doe.com"))))
    (is (= (:id (find-user "john@doe.com"))
           (get-in (find-login "BCDF-GHJK") [:cli-login/user :id]))
        "The login stays with the user who approved it")))

(dbtest redeem-an-approved-login
  (with-context approved-ctx
    (let [{:keys [auth-token]} (cli-logins/redeem "the-device-code-0123456789abcdef"
                                                  secret)]
      (is (= (:id (find-user "john@doe.com"))
             (unserialize-id (:user-id (read-token auth-token secret))))
          "The token identifies the approving user")
      (is (nil? (find-login "BCDF-GHJK"))
          "The login is removed")
      (is (= {:error "invalid_grant"}
             (cli-logins/redeem "the-device-code-0123456789abcdef" secret))
          "The token is issued only once"))))

(def ^:private expired-ctx
  (conj user-ctx
        #:cli-login{:device-code "the-device-code-0123456789abcdef"
                    :user-code "BCDF-GHJK"
                    :expires-at (t/minus (t/instant) (t/seconds 1))}))

(dbtest an-expired-login-cannot-be-approved
  (with-context expired-ctx
    (is (nil? (cli-logins/approve "BCDF-GHJK" (find-user "john@doe.com"))))
    (is (nil? (:cli-login/user (find-login "BCDF-GHJK"))))))

(dbtest an-expired-login-is-not-redeemed
  (with-context expired-ctx
    (is (= {:error "expired_token"}
           (cli-logins/redeem "the-device-code-0123456789abcdef" secret)))
    (is (nil? (find-login "BCDF-GHJK"))
        "The expired login is removed")))

(dbtest starting-a-login-removes-expired-ones
  (with-context expired-ctx
    (cli-logins/start)
    (is (nil? (find-login "BCDF-GHJK")))))
