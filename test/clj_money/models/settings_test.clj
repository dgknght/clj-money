(ns clj-money.models.settings-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.java.jdbc :as jdbc]
            [config.core :refer [env]]
            [clj-money.models.settings :as settings]))

(defn- clear-settings
  [f]
  (jdbc/delete! (env :db)
                :settings
                ["name in (?)" "My Info"])
  (f))

(use-fixtures :each clear-settings)

(deftest create-a-setting
  (let [created (settings/put "My Info" 24)
        retrieved (settings/get "My Info")]
    (is (comparable? {:name "My Info"
                      :value 24}
                     created)
        "put returns the created value")
    (is (= 24 retrieved)
        "The value can be retrieved")))

(deftest update-a-setting
  (let [_ (settings/put "My Info" 24)
        updated (settings/put "My Info" 25)
        retrieved (settings/get "My Info")]
    (is (comparable? {:name "My Info"
                      :value 25}
                     updated)
        "put returns the created record")
    (is (= 25 retrieved)
        "The value can be retrieved")))
