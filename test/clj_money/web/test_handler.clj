(ns clj-money.web.test-handler
  "Provides the application's Ring handler for tests. The handler is built
  without components, so requests use the storage bound by the test."
  (:require [clj-money.config :refer [env]]
            [clj-money.web.handler :as handler]))

(def ^:private handler
  (delay (handler/build {:env env})))

(defn app
  [req]
  (@handler req))
