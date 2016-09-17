(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.jdbc :as jdbc]
            [clj-money.web :refer :all])
  (:use [clj-money.models.accounts :as accounts]))

(def data-store (env :db))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (jdbc/with-db-connection [db data-store]
    (doseq [table ["accounts"]]
      (jdbc/execute! db (str "truncate table " table ";"))))
  (f))

(use-fixtures :each reset-db)

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create data-store {:name "Checking"
                                 :type :asset})
    (let [accounts (->> data-store
                        (accounts/select)
                        (map #(select-keys % [:name :type])))
          expected [{:name "Checking"
                     :type :asset}]]
      (is (= expected
             accounts)))))
