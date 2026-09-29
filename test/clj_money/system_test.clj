(ns clj-money.system-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.config :as config]
            [clj-money.system :as system]))

(deftest build-the-system-config-from-the-app-config
  (is (= {::system/env {:application-name "Test Money"}}
         (system/config {:application-name "Test Money"}))
      "The given application config is included in the system config")
  (is (= config/env
         (::system/env (system/config)))
      "The application config is used by default"))

(deftest initialize-the-system
  (let [sys (ig/init (system/config))]
    (try
      (is (= config/env (::system/env sys))
          "The application config is available in the running system")
      (finally
        (ig/halt! sys)))))

(deftest initialize-and-halt-the-system-with-namespace-loading
  (let [sys (system/init)]
    (is (= config/env (::system/env sys))
        "The full system is initialized")
    (system/halt sys))
  (let [sys (system/init (system/config) [::system/env])]
    (is (= [::system/env] (keys sys))
        "Only the specified keys are initialized")
    (system/halt sys)))
