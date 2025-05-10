(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.test :as test]
            [clj-money.decimal :as d]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.models :as models]))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (db/reset (db/storage))
  (f))

(defn- throw-if-nil
  [x msg]
  (when (nil? x)
    (throw (ex-info msg {})))
  x)

(defn account-ref
  [name]
  (-> {:account/name name}
      models/find-by
      (throw-if-nil (str "Account not found: " name))
      util/->model-ref))

(defn edn-body
  [req payload]
  (-> req
      (req/content-type "application/edn")
      (req/body (pr-str payload))))

(defn parse-edn-body
  [res]
  (test/parse-edn-body res :readers {'clj-money/local-date t/local-date
                                     'clj-money/local-date-time t/local-date-time
                                     'clj-money/decimal d/d}))
