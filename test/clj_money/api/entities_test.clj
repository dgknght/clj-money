(ns clj-money.api.entities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-entity]]
            [clj-money.api.entities :as api]
            [clj-money.models.entities :as entities]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private entity-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]
   :entities [{:user-id "john@doe.com"
               :name "Personal"}]})

(deftest update-an-entity
  (let [context (serialization/realize storage-spec entity-context)
        entity (find-entity context "Personal")
        response (with-authentication (-> context :users first)
                   (api/update {:params {:id (:id entity)
                                         :name "My Stuff"
                                         :settings {:inventory-method "fifo"}}}))
        retrieved (entities/reload storage-spec entity)]
    (is (= 200 (:status response))
        "The request is successful")
    (is (= "fifo" (-> response :body (json/parse-string true) :settings :inventory-method))
        "The response includes the updated inventory-method value")
    (is (= :fifo (-> retrieved :settings :inventory-method))
        "The record is updated")))
