(ns clj-money.api.cli-logins
  "Endpoints for signing in the command-line app through the browser. See
  clj-money.cli-logins for the flow."
  (:require [dgknght.app-lib.api :as api]
            [clj-money.config :refer [env]]
            [clj-money.web.system :as system]
            [clj-money.cli-logins :as cli-logins]))

(defn- verification-uri []
  (str (env :site-protocol) "://" (env :site-host) "/cli/login"))

(defn- start
  [_req]
  (let [{:cli-login/keys [device-code user-code]} (cli-logins/start)
        verification-uri (verification-uri)]
    (api/response {:device-code device-code
                   :user-code user-code
                   :verification-uri verification-uri
                   :verification-uri-complete (str verification-uri
                                                   "?code="
                                                   user-code)
                   :expires-in cli-logins/lifetime-seconds
                   :interval cli-logins/poll-interval-seconds}
                  201)))

(defn- token
  [{:keys [params] :as req}]
  ; JSON keys arrive as sent: deviceCode, by the API's JSON convention
  (let [result (cli-logins/redeem (some params [:device-code :deviceCode])
                                  (-> req
                                      (system/component :services)
                                      :auth-secret))]
    (if (:auth-token result)
      (api/response result)
      (api/response result 400))))

(defn- approve
  [{:keys [path-params authenticated]}]
  (if (cli-logins/approve (:user-code path-params) authenticated)
    api/no-content
    api/not-found))

(def unauthenticated-routes
  ; The CLI has no credentials until it gets its token, and the device code
  ; it polls with is the secret that guards the token.
  ["cli-logins"
   ["" {:post {:handler start}}]
   ["/token" {:post {:handler token}}]])

(def routes
  ["cli-logins/:user-code/approve" {:post {:handler approve}}])
