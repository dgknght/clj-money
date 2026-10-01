(ns clj-money.services-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.services :as services]))

(def ^:private app-config
  {:application-name "Test Money"
   :site-protocol "https"
   :site-host "money.example.com"
   :mailer-enabled? true
   :mailer-host "mail.example.com"
   :mailer-from "no-reply@example.com"
   :honeybadger-api-key "hb-key"
   :secret "jwt-secret"
   :google-client-id "google-id"
   :google-client-secret "google-secret"
   :github-client-id "github-id"
   :github-client-secret "github-secret"
   :yahoo-api-key "yahoo-key"
   :alpha-vantage-api-key "alpha-key"})

(deftest build-the-service-config-from-the-app-config
  (is (= {:mailer {:enabled? true
                   :host "mail.example.com"
                   :from "no-reply@example.com"
                   :app-name "Test Money"
                   :site-url "https://money.example.com"}
          :honeybadger {:api-key "hb-key"
                        :environment-name "production"}
          :auth-secret "jwt-secret"
          :oauth {:google {:client-id "google-id"
                           :client-secret "google-secret"}
                  :github {:client-id "github-id"
                           :client-secret "github-secret"}}
          :yahoo-api-key "yahoo-key"
          :alpha-vantage-api-key "alpha-key"}
         (services/config app-config))
      "The service config is extracted from the app config")
  (is (= "development"
         (get-in (services/config (assoc app-config :dev? true))
                 [:honeybadger :environment-name]))
      "The HoneyBadger environment name reflects the dev flag"))

(deftest initialize-the-service-config
  (let [config (services/config app-config)
        sys (ig/init {::services/config config})]
    (is (= config (::services/config sys))
        "The component is the service config")))
