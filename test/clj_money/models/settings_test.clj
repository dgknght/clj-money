(ns clj-money.models.settings-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.java.jdbc :as jdbc]
            [environ.core :refer [env]]
            [clj-money.test-helpers :refer [selective=]]
            [clj-money.models.settings :as settings]))

(defn- clear-settings
  [f]
  (jdbc/delete! (env :db)
                :settings
                ["name in (?)" "My Info"])
  (f))

(use-fixtures :each clear-settings)

(deftest create-a-setting
  (let [created (settings/put (env :db) "My Info" 24)
        retrieved (settings/get (env :db) "My Info")]
    (is (selective= {:name "My Info"
                     :value 24}
                    created)
        "put returns the created value")
    (is (= 24 retrieved)
        "The value can be retrieved")))

(deftest update-a-setting
  (let [_ (settings/put (env :db) "My Info" 24)
        updated (settings/put (env :db) "My Info" 25)
        retrieved (settings/get (env :db) "My Info")]
    (is (selective= {:name "My Info"
                     :value 25}
                    updated)
        "put returns the created record")
    (is (= 25 retrieved)
        "The value can be retrieved")))
