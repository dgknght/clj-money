(ns clj-money.system-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.config :as config]
            [clj-money.db :as db]
            [clj-money.images :as images]
            [clj-money.progress :as progress]
            [clj-money.system :as system]
            [clj-money.web :as-alias web]))

(deftest build-the-system-config-from-the-app-config
  (let [app-config {:application-name "Test Money"
                    :db {:strategies {:sql {:dbname "money_test"}}
                         :active :sql}
                    :image-storage {:bucket "test-images"}
                    :progress {:strategies {:redis {:prefix "test"}}
                               :active :redis}}]
    (is (= {::system/env app-config
            ::db/storage {:dbname "money_test"}
            ::images/storage {:bucket "test-images"}
            ::progress/tracker-factory {:prefix "test"}
            ::web/handler {:env (ig/ref ::system/env)
                           :storage (ig/ref ::db/storage)
                           :image-storage (ig/ref ::images/storage)
                           :tracker-factory (ig/ref ::progress/tracker-factory)}}
           (system/config app-config))
        "The system config includes the app config and the storage configs"))
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
      (is (satisfies? images/Storage (::images/storage sys))
          "The image storage is available in the running system")
      (is (satisfies? progress/TrackerFactory (::progress/tracker-factory sys))
          "The progress tracker factory is available in the running system")
      (is (fn? (::web/handler sys))
          "The web handler is available in the running system")
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
