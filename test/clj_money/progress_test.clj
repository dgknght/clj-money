(ns clj-money.progress-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.config :refer [env]]
            [clj-money.progress :as prog]
            [clj-money.progress.redis]))

(defmethod prog/reify-tracker ::fake
  [config root-key]
  (reify prog/Tracker
    (expect [_ _ _])
    (increment [_ _])
    (increment [_ _ _])
    (get [_] {:config config :root root-key})
    (warn [_ _])
    (fail [_ _])
    (finish [_])))

(deftest get-the-active-tracker-config
  (is (= {::prog/strategy ::fake}
         (prog/active-config {:progress {:strategies {:fake {::prog/strategy ::fake}}
                                         :active :fake}}))
      "The config for the active strategy is returned")
  (is (= ::prog/redis
         (::prog/strategy (prog/active-config env)))
      "The configured strategy is returned for the application config"))

(deftest create-trackers-with-the-default-factory-method
  (let [factory (prog/reify-tracker-factory {::prog/strategy ::fake})]
    (is (= {:config {::prog/strategy ::fake} :root 101}
           (prog/get (prog/create-tracker factory 101)))
        "The tracker is created from the config and root key")
    (is (nil? (prog/close factory))
        "Closing the factory succeeds")))

(deftest create-a-tracker-with-the-bound-factory
  (binding [prog/*tracker-factory* (prog/reify-tracker-factory {::prog/strategy ::fake})]
    (is (= {:config {::prog/strategy ::fake} :root 101}
           (prog/get (prog/tracker 101)))
        "The bound factory creates the tracker")))

(deftest reuse-the-default-factory-when-none-is-bound
  (is (identical? (prog/tracker-factory) (prog/tracker-factory))
      "The same factory is returned on each call"))

(deftest initialize-and-halt-a-tracker-factory-component
  (let [closed? (atom false)
        factory (reify prog/TrackerFactory
                  (create-tracker [_ _])
                  (close [_] (reset! closed? true)))]
    (with-redefs [prog/reify-tracker-factory (constantly factory)]
      (let [sys (ig/init {::prog/tracker-factory {::prog/strategy ::fake}})]
        (is (identical? factory (::prog/tracker-factory sys))
            "The factory is created from the config")
        (ig/halt! sys)
        (is @closed?
            "The factory is closed when the system is halted")))))
