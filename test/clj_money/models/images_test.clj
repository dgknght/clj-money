(ns clj-money.models.images-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.images :as images]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def image-context
  {:users [(factory :user)]})

(defn attributes
  [context]
  {:user-id (-> context :users first :id)
   :original-filename "sample.gnucash"
   :body-hash "dd9c277e9bfdbdace10a06820e76b659a6b3a851"
   :body (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))})

(deftest create-an-image
  (let [context (serialization/realize storage-spec image-context)
        result (images/create storage-spec (attributes context))]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (:id result)
        "The result has an :id value")))

; user-id is required
; original_filename is required
; body_hash is required
; body_hash is unique for user_id
; body is required
