(ns clj-money.honeybadger
  (:require [clojure.tools.logging :as log]
            [cheshire.core :as json]
            [clj-http.client :as http]))

(defn- format-frame
  [^StackTraceElement frame]
  {:number (str (.getLineNumber frame))
   :file (or (.getFileName frame) "unknown")
   :method (format "%s.%s" (.getClassName frame) (.getMethodName frame))})

(defn- exception->payload
  [^Throwable e {:keys [environment-name]}]
  {:notifier {:name "clj-money"
              :version "1.0.0"
              :url "https://github.com/dgknght/clj-money"}
   :error {:class (.getName (.getClass e))
           :message (or (ex-message e) "")
           :backtrace (mapv format-frame (.getStackTrace e))}
   :server {:environment_name environment-name
            :hostname (.. java.net.InetAddress getLocalHost getHostName)}})

(defn notify
  "Reports the exception to HoneyBadger. The config is the HoneyBadger
  configuration from clj-money.services: :api-key and :environment-name.
  Does nothing when there is no API key."
  [^Throwable e config]
  (when-let [api-key (:api-key config)]
    (try
      (http/post "https://api.honeybadger.io/v1/notices"
                 {:headers {"X-API-Key" api-key
                            "Content-Type" "application/json"
                            "Accept" "application/json"}
                  :body (json/generate-string (exception->payload e config))
                  :throw-exceptions false})
      (catch Exception err
        (log/error err "Failed to notify HoneyBadger")))))
