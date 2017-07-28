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
  {:users [(factory :user)]
   :images [{:original-filename "somefile.gnucash"
             :content-type "application/gnucash"
             :body "resources/fixtures/sample.gnucash"}]})

(defn attributes
  [context]
  {:user-id (-> context :users first :id)
   :entity-name "Personal"
   :image-id (-> context :images first :id)})

(deftest create-an-import
  (let [context (serialization/realize storage-spec import-context)
        result (imports/create storage-spec (attributes context))]

    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (:id result) "It assigns an ID to the result")))

(deftest user-id-is-required
  (let [context (serialization/realize storage-spec import-context)
        result (imports/create storage-spec
                               (dissoc (attributes context) :user-id))]
    (is (not (empty? (validation/error-messages result :user-id)))
        "There is a validation error on :user-id")))

(deftest image-id-is-required
  (let [context (serialization/realize storage-spec import-context)
        result (imports/create storage-spec
                               (dissoc (attributes context) :image-id))]
    (is (not (empty? (validation/error-messages result :image-id)))
        "There is a validation error on :image-id")))

(deftest entity-name-is-required
  (let [context (serialization/realize storage-spec import-context)
        result (imports/create storage-spec
                               (dissoc (attributes context) :entity-name))]
    (is (not (empty? (validation/error-messages result :entity-name)))
        "There is a validation error on :entity-name")))

(deftest update-an-import
  (let [context (serialization/realize storage-spec import-context)
        import (imports/create storage-spec (attributes context))
        updated (assoc import :progress {:account {:total 20
                                                   :processed 0}})
        result (try
                 (imports/update storage-spec updated)
                 (catch java.sql.BatchUpdateException e
                   (pprint {:error (.getNextException e)})))
        retrieved (imports/find-by-id storage-spec (:id import))]
    (is (= {:account {:total 20
                      :processed 0}}
           (:progress retrieved))
        "The correct value is retrieved after update")))
