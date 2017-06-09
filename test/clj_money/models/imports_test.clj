(ns clj-money.models.imports-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.imports :as imports]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def import-context
  {:users [(factory :user)]})

(defn attributes
  [context]
  {:user-id (-> context :users first :id)
   :entity-name "Personal"
   :source-file (io/input-stream "resources/fixtures/sample.gnucash")})

(deftest create-an-import
  (let [context (serialization/realize storage-spec import-context)
        result (imports/create storage-spec (attributes context))]
    (is (:id result) "It assigns an ID to the result")))

(deftest user-id-is-required
  (is false "need to write the test"))

(deftest source-file-is-required
  (is false "need to write the test"))

(deftest entity-name-is-required
  (is false "need to write the test"))

(deftest update-an-import
  (let [context (serialization/realize storage-spec import-context)
        import (imports/create storage-spec (attributes context))
        updated (assoc import :record-counts {:accounts {:total 20
                                                         :processed 0}})
        result (try
                 (imports/update storage-spec updated)
                 (catch java.sql.BatchUpdateException e
                   (pprint {:error (.getNextException e)})))
        retrieved (imports/find-by-id storage-spec (:id import))]
    (is (= {:accounts {:total 20
                       :processed 0}}
           (:record-counts retrieved))
        "The correct value is retrieved after update")))
