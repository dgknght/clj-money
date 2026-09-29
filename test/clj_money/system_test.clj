(ns clj-money.system-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.config :as config]
            [clj-money.db :as db]
            [clj-money.system :as system]))

(deftest build-the-system-config-from-the-app-config
  (let [app-config {:application-name "Test Money"
                    :db {:strategies {:sql {:dbname "money_test"}}
                         :active :sql}}]
    (is (= {::system/env app-config
            ::db/storage {:dbname "money_test"}}
           (system/config app-config))
        "The system config includes the app config and the active storage config"))
  (is (= config/env
         (::system/env (system/config)))
      "The application config is used by default"))

(deftest initialize-the-system
  (let [sys (ig/init (system/config))]
    (try
      (is (= config/env (::system/env sys))
          "The application config is available in the running system")
      (is (satisfies? db/Storage (::db/storage sys))
          "The storage is available in the running system")
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
