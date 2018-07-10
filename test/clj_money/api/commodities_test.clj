(ns clj-money.api.commodities-test
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
                                            find-user
                                            find-entity
                                            find-commodity]]
            [clj-money.api.commodities :as api]
            [clj-money.models.commodities :as commodities])
  (:import clojure.lang.ExceptionInfo))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private commodities-context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]})

(deftest get-a-list-of-commodities
  (let [context (serialization/realize storage-spec commodities-context)
        entity (find-entity context "Personal")]
    (testing "A user can list his own entities"
      (let [user (find-user context "john@doe.com")
            response (with-authentication user
                   (api/index {:params {:entity-id (:id entity)}}))]
        (is (= 200 (:status response)) "The response is successful")
        (is (= #{"USD"} (->> (:body response)
                             (map :symbol)
                             (into #{})))
            "The response contains the commodities.")))
    (testing "A user cannot get a list of someone else's entities"
      (let [user (find-user context "jane@doe.com")]
        (is (thrown? ExceptionInfo
                     (with-authentication user
                       (api/index {:params {:entity-id (:id entity)}}))))))))

(defn- commodity-attributes
  [entity-id]
  {:entity-id entity-id
   :type "stock"
   :name "Apple, Inc."
   :symbol "AAPL"
   :exchange "nasdaq"})

(deftest create-a-commodity
  (let [context (serialization/realize storage-spec commodities-context)
        entity (find-entity context "Personal")]
    (testing "A user cannot create a commodity for someone else's entities"
      (let [user (find-user context "jane@doe.com")
            _ (is (thrown? ExceptionInfo
                           (with-authentication user
                             (api/create {:params (commodity-attributes (:id entity))})))) 
            commodities (commodities/search storage-spec {:entity-id (:id entity)})]
        (is (not ((->> commodities
                      (map :symbol)
                      (into #{}))
                  "AAPL"))
            "The commodity is not created.")))
    (testing "A user can create a commodity for his own entity"
      (let [user (find-user context "john@doe.com")
            response (with-authentication user
                       (api/create {:params (commodity-attributes (:id entity))}))]
        (is (= 201 (:status response)) "The response is a successful creation")
        (is (-> response :body :id)
            "The response contains the new commodity id.")))))

(deftest update-a-commodity
  (let [context (serialization/realize storage-spec commodities-context)
        commodity (find-commodity context "USD")]
    (testing "A user cannot update a commodity for another user's entity"
      (is (thrown? ExceptionInfo
                   (with-authentication (find-user context "jane@doe.com")
                     (api/update {:params {:id (:id commodity)
                                           :name "US Doll Hairs"
                                           :symbol "USD"
                                           :type "currency"}})))))
    (testing "A user can update a commodity in their entity"
      (let [response (with-authentication (find-user context "john@doe.com")
                     (api/update {:params {:id (:id commodity)
                                           :name "US Doll Hairs"
                                           :symbol "USD"
                                           :type "currency"}}))
            entity (find-entity context "Personal")
            commodities (commodities/search storage-spec {:entity-id (:id entity)})]
        (is (= 200 (:status response)) "The response is successful")
        (is (= #{"US Doll Hairs"} (->> commodities
                                       (map :name)
                                       (into #{})))
            "The commodity is updated in the data store.")))))

(deftest delete-a-commodity
  (let [context (serialization/realize storage-spec commodities-context)
        entity (find-entity context "Personal")
        commodity (find-commodity context "USD")]
    (testing "A user cannot delete a commodity from another user's entity"
      (let [_ (is (thrown? ExceptionInfo
                   (with-authentication (find-user context "jane@doe.com")
                     (api/delete {:params {:id (:id commodity)}}))))
            commodities (commodities/search storage-spec {:entity-id (:id entity)})]
        (is (= #{"USD"} (->> commodities
                             (map :symbol)
                             (into #{})))
            "The commodity is stil retrievable after attempt to delete")))
    (testing "A user can delete a commodity from their entity"
      (let [response (with-authentication (find-user context "john@doe.com")
                       (api/delete {:params {:id (:id commodity)}}))
            commodities (commodities/search storage-spec {:entity-id (:id entity)})]
        (is (= 204 (:status response)) "The response is successful and empty")
        (is (not ((->> commodities
                       (map :symbol)
                       (into #{}))
                  "USD"))
            "The commodity is not retrieved after delete")))))
