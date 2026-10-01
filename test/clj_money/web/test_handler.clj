(ns clj-money.web.test-handler
  "Provides the application's Ring handler for tests. The handler is built
  without storage components, so requests use the storage bound by the
  test, and with the external service configuration from the test config."
  (:require [clj-money.config :refer [env]]
            [clj-money.services :as services]
            [clj-money.web.handler :as handler]))

(def ^:private handler
  (delay (handler/build {:env env
                         :services (services/config env)})))

(defn app
  [req]
  (@handler req))
