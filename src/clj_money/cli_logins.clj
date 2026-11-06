(ns clj-money.cli-logins
  "Signs in the command-line app through the browser, in the manner of the
  OAuth 2.0 device authorization grant (RFC 8628):

  1. The CLI starts a login and gets a secret device code and a short user
     code, then opens the browser at the approval page with the user code.
  2. The user signs in to the web app, if they aren't already, and approves
     the login, which links it to them.
  3. Meanwhile the CLI polls with the device code. Once the login is
     approved, the poll returns an auth token and the login is removed, so
     the token is issued only once."
  (:require [clojure.string :as string]
            [java-time.api :as t]
            [clj-money.entities :as entities]
            [clj-money.entities.cli-logins]
            [clj-money.web.auth :refer [make-token]])
  (:import [java.security SecureRandom]))

(def lifetime-seconds
  "How long the CLI and the user have to complete a login"
  600)

(def poll-interval-seconds
  "How long the CLI should wait between polls"
  5)

; Consonants only, so a code can't spell a word, and none that are easily
; mistaken for a digit or another letter. 20^8 codes.
(def ^:private user-code-chars "BCDFGHJKLMNPQRSTVWXZ")

(def ^:private random (SecureRandom.))

(defn- random-chars
  [n]
  (apply str (repeatedly n #(nth user-code-chars
                                 (.nextInt random (count user-code-chars))))))

(defn- user-code []
  (str (random-chars 4) "-" (random-chars 4)))

(defn- device-code []
  (let [bytes (byte-array 32)]
    (.nextBytes random bytes)
    (apply str (map #(format "%02x" %) bytes))))

(defn- expired?
  [{:cli-login/keys [expires-at]}]
  (not (t/before? (t/instant) (t/instant expires-at))))

(defn- delete-expired []
  (when-let [expired (seq (entities/select {:cli-login/expires-at [:<= (t/instant)]}))]
    (entities/delete-many expired)))

(defn start
  "Starts a login, returning the saved login."
  []
  (delete-expired)
  (entities/put #:cli-login{:device-code (device-code)
                            :user-code (user-code)
                            :expires-at (t/plus (t/instant)
                                                (t/seconds lifetime-seconds))}))

(defn- normalize-user-code
  [code]
  (some-> code string/trim string/upper-case))

(defn approve
  "Links the pending login with the given user code to the user, returning
  the updated login, or nil if there's no such login, it has expired, or it
  was already approved."
  [user-code user]
  (when-let [login (some->> (normalize-user-code user-code)
                            (hash-map :cli-login/user-code)
                            entities/find-by)]
    (when-not (or (expired? login)
                  (:cli-login/user login))
      (entities/put (assoc login :cli-login/user user)))))

(defn redeem
  "Exchanges the device code of an approved login for an auth token, signed
  with the given secret, removing the login. Returns {:auth-token token} on
  success, otherwise {:error code}, where code is one of the RFC 8628 error
  codes the CLI should act on:

  - authorization_pending: the user hasn't approved it yet; poll again
  - expired_token: the login expired; start over
  - invalid_grant: there's no such login, or its token was already issued"
  [device-code secret]
  (let [login (when (string? device-code)
                (entities/find-by {:cli-login/device-code device-code}))]
    (cond
      (nil? login)
      {:error "invalid_grant"}

      (expired? login)
      (do (entities/delete login)
          {:error "expired_token"})

      (nil? (:cli-login/user login))
      {:error "authorization_pending"}

      :else
      (do (entities/delete login)
          {:auth-token (make-token (:cli-login/user login) secret)}))))
