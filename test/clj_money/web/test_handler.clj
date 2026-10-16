(ns clj-money.web.test-handler
  "Provides the application's Ring handler for tests. The handler is built
  without storage components, so requests use the storage bound by the
  test, and with the external service configuration from the test config.

  Propagation is asynchronous, so app waits for the changes made by each
  request to be propagated before returning the response, so that tests
  can assert against the propagated state."
  (:require [clj-money.config :refer [env]]
            [clj-money.test-helpers :refer [propagated]]
            [clj-money.services :as services]
            [clj-money.web.handler :as handler]))

(defn build-app
  "Returns a handler built with the given components (e.g. {:ingestion
  reader}) in addition to the default ones."
  [components]
  (handler/build (merge {:env env
                         :services (services/config env)}
                        components)))

(def ^:private handler
  (delay (build-app {})))

(defn app
  [req]
  (propagated (@handler req)))
