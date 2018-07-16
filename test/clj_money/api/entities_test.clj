(ns clj-money.api.entities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [clj-money.api.test-helper :refer [deftest-delete]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-user
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

(deftest create-an-entity
  (let [context (serialization/realize storage-spec entity-context)
        user (find-user context "john@doe.com")
        response (with-authentication user
                   (api/create {:params {:name "Business"
                                         :settings {:inventory-method "fifo"}}}))
        retrieved (entities/select storage-spec (:id user))]
    (is (= 201 (:status response)) "The response is a successful creation")
    (is (= {:name "Business"
            :settings {:inventory-method :fifo}}
           (select-keys (:body response) [:name :settings]))
        "The new entity is returned in the response")
    (is (= #{"Personal" "Business"} (->> retrieved
                                         (map :name)
                                         (into #{})))
        "The new entity can be retrieved.")))

(def ^:private entities-context
  (-> entity-context
      (update-in [:entities] #(concat % [{:user-id "john@doe.com"
                                          :name "Business"}
                                         {:user-id "jane@doe.com"
                                          :name "Other"}]))))

(deftest get-a-list-of-entities
  (let [context (serialization/realize storage-spec entities-context)
        response (with-authentication (-> context :users first)
                   (api/index {:params {}}))]
    (is (= 200 (:status response)) "The response is successful")
    (is (= #{"Business" "Personal"} (->> (:body response)
                                         (map :name)
                                         (into #{})))
        "The correct entities are returned.")))

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

(deftest-delete delete-an-entity
  entities-context
  {:resource-name "entity"
   :storage storage-spec
   :find-resource-fn #(find-entity % "Personal")
   :find-user-fn #(find-user % "john@doe.com")
   :find-other-user-fn #(find-user % "jane@doe.com")
   :delete-fn api/delete
   :select-resources-fn #(entities/select storage-spec (:id (find-user % "john@doe.com")))})
