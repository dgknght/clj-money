(ns clj-money.system-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.config :as config]
            [clj-money.db :as db]
            [clj-money.images :as images]
            [clj-money.progress :as progress]
            [clj-money.services :as services]
            [clj-money.system :as system]
            [clj-money.web :as-alias web]
            [clj-money.web.server]))

; Port 0 lets Jetty choose any free port
(def ^:private test-env
  (assoc config/env :port 0))

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
            ::services/config (services/config app-config)
            ::web/handler {:env (ig/ref ::system/env)
                           :storage (ig/ref ::db/storage)
                           :image-storage (ig/ref ::images/storage)
                           :tracker-factory (ig/ref ::progress/tracker-factory)
                           :services (ig/ref ::services/config)}
            ::web/server {:handler (ig/ref ::web/handler)
                          :port 3000}}
           (system/config app-config))
        "The system config includes the app config, the storage configs and the service config")
    (is (= 8080 (get-in (system/config (assoc app-config :port "8080"))
                        [::web/server :port]))
        "The port is read from the app config"))
  (is (= config/env
         (::system/env (system/config)))
      "The application config is used by default"))

(deftest initialize-the-system
  (let [sys (ig/init (system/config test-env))]
    (try
      (is (= test-env (::system/env sys))
          "The application config is available in the running system")
      (is (satisfies? db/Storage (::db/storage sys))
          "The storage is available in the running system")
      (is (satisfies? images/Storage (::images/storage sys))
          "The image storage is available in the running system")
      (is (satisfies? progress/TrackerFactory (::progress/tracker-factory sys))
          "The progress tracker factory is available in the running system")
      (is (= (services/config test-env) (::services/config sys))
          "The external service config is available in the running system")
      (is (fn? (::web/handler sys))
          "The web handler is available in the running system")
      (is (.isStarted (::web/server sys))
          "The web server is running in the running system")
      (finally
        (ig/halt! sys)))))

(deftest initialize-and-halt-the-system-with-namespace-loading
  (let [sys (system/init (system/config test-env))]
    (is (= test-env (::system/env sys))
        "The full system is initialized")
    (system/halt sys)
    (is (.isStopped (::web/server sys))
        "The web server is stopped when the system is halted"))
  (let [sys (system/init (system/config) [::system/env])]
    (is (= [::system/env] (keys sys))
        "Only the specified keys are initialized")
    (system/halt sys)))

(deftest bind-the-components-of-a-system
  (is (= [::storage ::image-storage ::tracker-factory]
         (system/with-components {::db/storage ::storage
                                  ::images/storage ::image-storage
                                  ::progress/tracker-factory ::tracker-factory}
           [db/*storage* images/*storage* progress/*tracker-factory*]))
      "The system's components are bound")
  (is (= [::outer-storage nil nil]
         (binding [db/*storage* ::outer-storage]
           (system/with-components nil
             [db/*storage* images/*storage* progress/*tracker-factory*])))
      "Existing bindings are kept when there is no system"))

(defmethod ig/init-key ::probe
  [_ events]
  (swap! events conj :init)
  events)

(defmethod ig/halt-key! ::probe
  [_ events]
  (swap! events conj :halt))

(deftest run-with-a-partial-system
  (let [sys-keys (atom nil)
        storage (atom nil)
        bound-storage (atom nil)]
    (system/with-system [sys [::db/storage]]
      (reset! sys-keys (set (keys sys)))
      (reset! storage (::db/storage sys))
      (reset! bound-storage db/*storage*))
    (is (= #{::db/storage} @sys-keys)
        "Only the specified keys are initialized")
    (is (identical? @storage @bound-storage)
        "The system's storage is bound while the body is evaluated")))

(deftest halt-the-partial-system-afterward
  (let [events (atom [])]
    (is (= :result
           (system/with-system [_ [::probe] {::probe events}]
             :result))
        "The value of the body is returned")
    (is (= [:init :halt] @events)
        "The system is halted after the body is evaluated")
    (reset! events [])
    (is (thrown? RuntimeException
                 (system/with-system [_ [::probe] {::probe events}]
                   (throw (RuntimeException. "boom")))))
    (is (= [:init :halt] @events)
        "The system is halted when the body throws")))

(defmethod ig/init-key ::failing
  [_ _]
  (throw (RuntimeException. "Induced failure")))

(deftest halt-the-started-components-when-initialization-fails
  (let [events (atom [])
        cfg {::probe events
             ::failing {:probe (ig/ref ::probe)}}
        ex (try
             (system/init cfg)
             nil
             (catch clojure.lang.ExceptionInfo e
               e))]
    (is (= "Induced failure" (some-> ex ex-cause ex-message))
        "The initialization failure is rethrown")
    (is (= [:init :halt] @events)
        "The components already started are halted")))

(deftest exit-when-the-server-cannot-start
  (with-open [socket (java.net.ServerSocket. 0)]
    (let [status (atom nil)]
      (with-redefs [clj-money.config/env test-env
                    clj-money.web.server/exit #(reset! status %)]
        (clj-money.web.server/-main (str (.getLocalPort socket))))
      (is (= 1 @status)
          "The process exits with a non-zero status"))))
