(ns clj-money.api.cli-auth
  (:require [dgknght.app-lib.api :as api]
            [clj-money.entities.cli-auth-sessions :as cli-sessions]
            [clj-money.web.auth :as auth]
            [clj-money.config :refer [env]]))

(def ^:private verification-uri
  (str (:site-protocol env) "://" (:site-host env) "/cli/authorize"))

(def ^:private polling-interval 5)
(def ^:private token-expiration 900) ; 15 minutes in seconds

(defn- initiate-device-flow
  "Creates a new CLI auth session and returns device authorization data"
  [_request]
  (let [session (cli-sessions/create-session)]
    (api/response
     {:device-code (:cli-auth-session/device-code session)
      :user-code (:cli-auth-session/user-code session)
      :verification-uri verification-uri
      :verification-uri-complete (str verification-uri
                                      "?user_code="
                                      (:cli-auth-session/user-code session))
      :expires-in token-expiration
      :interval polling-interval})))

(defn- poll-for-token
  "Polls for an access token using the device code"
  [{:keys [body-params]}]
  (let [{:keys [device-code]} body-params]
    (if-let [session (cli-sessions/find-by-device-code device-code)]
      (cond
        (cli-sessions/expired? session)
        (api/response {:error "expired_token"
                       :error-description
                       "The device code has expired"}
                      400)

        (= :denied (:cli-auth-session/status session))
        (api/response {:error "access_denied"
                       :error-description
                       "The user denied the authorization request"}
                      403)

        (= :approved (:cli-auth-session/status session))
        (let [user {:id (:cli-auth-session/user-id session)}
              token (auth/make-token user)]
          (api/response {:access-token token
                         :token-type "Bearer"
                         :expires-in token-expiration}))

        :else ; :pending
        (api/response {:error "authorization_pending"
                       :error-description
                       "The user has not yet completed authorization"}
                      400))
      (api/response {:error "invalid_request"
                     :error-description "Invalid device_code"}
                    400))))

(defn- approve-session
  "Approves a CLI auth session for the authenticated user"
  [{:keys [authenticated body-params]}]
  (let [{:keys [user-code]} body-params]
    (if-let [session (cli-sessions/find-by-user-code user-code)]
      (cond
        (cli-sessions/expired? session)
        (api/response {:error "Session has expired"} 400)

        (not= :pending (:cli-auth-session/status session))
        (api/response {:error "Session has already been processed"}
                      400)

        :else
        (do
          (cli-sessions/approve-session session authenticated)
          (api/response {:success true
                         :message "CLI authenticated successfully"})))
      (api/not-found "Invalid user code"))))

(defn- deny-session
  "Denies a CLI auth session"
  [{:keys [body-params]}]
  (let [{:keys [user-code]} body-params]
    (if-let [session (cli-sessions/find-by-user-code user-code)]
      (cond
        (cli-sessions/expired? session)
        (api/response {:error "Session has expired"} 400)

        (not= :pending (:cli-auth-session/status session))
        (api/response {:error "Session has already been processed"}
                      400)

        :else
        (do
          (cli-sessions/deny-session session)
          (api/response {:success true
                         :message "Authorization denied"})))
      (api/not-found "Invalid user code"))))

(def unauthenticated-routes
  ["cli/auth"
   ["/device" {:post {:handler initiate-device-flow}}]
   ["/token" {:post {:handler poll-for-token}}]])

(def authenticated-routes
  ["cli/auth"
   ["/approve" {:post {:handler approve-session}}]
   ["/deny" {:post {:handler deny-session}}]])
