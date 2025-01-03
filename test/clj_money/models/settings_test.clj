(ns clj-money.models.settings-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [java-time.api :as t]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :refer [delete!]]
            [config.core :refer [env]]
            [dgknght.app-lib.test-assertions]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.models.settings :as settings]))

(defn- clear-settings
  [f]
  (let [ds (jdbc/get-datasource (get-in env [:db :strategies :sql]))]
    (delete! ds
             :settings
             {:name "my-info"}))
  (f))

(use-fixtures :each clear-settings)

(deftest save-a-string-setting
  (settings/put :my-info "testing 1, 2, 3")
  (is (= "testing 1, 2, 3"
         (settings/get :my-info))
      "A new setting can be inserted")
  (settings/put :my-info "another")
  (is (= "another"
         (settings/get :my-info))
      "An existing setting can be updated"))

(deftest save-a-number-setting
  (settings/put :my-info 25)
  (is (= 25
         (settings/get :my-info))
      "A new setting can be inserted")
  (settings/put :my-info 50)
  (is (= 50
         (settings/get :my-info))
      "An existing setting can be updated"))

(deftest save-a-date-setting
  (settings/put :my-info (t/local-date 2020 3 2))
  (is (= (t/local-date 2020 3 2)
         (settings/get :my-info))
      "A new setting can be inserted")
  (settings/put :my-info (t/local-date 2020 2 27))
  (is (= (t/local-date 2020 2 27)
         (settings/get :my-info))
      "An existing setting can be updated"))
