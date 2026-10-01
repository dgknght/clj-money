(ns clj-money.services
  "Configuration for the external services the application uses (mail,
  error reporting, auth tokens, OAuth providers and price quote APIs).

  The configuration is built from the application configuration and
  managed as the ::config Integrant component. It is passed to the functions
  that use it as an argument (see clj-money.web.system for how request
  handlers get it)."
  (:require [integrant.core :as ig]))

(defn config
  "Returns the external service configuration from the given application
  configuration."
  [env]
  {:mailer {:enabled? (:mailer-enabled? env)
            :host (:mailer-host env)
            :from (:mailer-from env)
            :app-name (:application-name env)
            :site-url (str (:site-protocol env) "://" (:site-host env))}
   :honeybadger {:api-key (:honeybadger-api-key env)
                 :environment-name (if (:dev? env) "development" "production")}
   :auth-secret (:secret env)
   :oauth {:google {:client-id (:google-client-id env)
                    :client-secret (:google-client-secret env)}
           :github {:client-id (:github-client-id env)
                    :client-secret (:github-client-secret env)}}
   :yahoo-api-key (:yahoo-api-key env)
   :alpha-vantage-api-key (:alpha-vantage-api-key env)})

(defmethod ig/init-key ::config
  [_ config]
  config)
