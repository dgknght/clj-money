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
