(ns clj-money.api.commodities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [clj-money.api.test-helper :refer [deftest-create
                                               deftest-delete
                                               deftest-update
                                               deftest-list]]
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

(deftest-list get-a-list-of-commodities
  commodities-context
  {:resource-name "commodity"
   :storage storage-spec
   :find-user-fn #(find-user % "john@doe.com")
   :find-other-user-fn #(find-user % "jane@doe.com")
   :list-fn api/index
   :params-fn #(hash-map :entity-id (:id (find-entity % "Personal")))
   :expectation-fn #(= #{"USD"} (->> %
                                     (map :symbol)
                                     (into #{})))})

(def ^:private commodity-attributes
  {:type "stock"
   :name "Apple, Inc."
   :symbol "AAPL"
   :exchange "nasdaq"})

(deftest-create create-a-commodity
  commodities-context
  {:resource-name "commodity"
   :storage storage-spec
   :find-user-fn #(find-user % "john@doe.com")
   :find-other-user-fn #(find-user % "jane@doe.com")
   :create-fn api/create
   :create-params-fn #(assoc commodity-attributes :entity-id (:id (find-entity % "Personal")))
   :select-resources-fn #(commodities/search storage-spec {:entity-id (:id (find-entity % "Personal"))})
   :compare-fn #(= (:symbol %) "AAPL")})

(deftest-update update-a-commodity
  commodities-context
  {:resource-name "commodity"
   :storage storage-spec
   :find-resource-fn #(find-commodity % "USD")
   :find-updated-resource-fn #(commodities/find-by-id storage-spec %)
   :find-user-fn #(find-user % "john@doe.com")
   :find-other-user-fn #(find-user % "jane@doe.com")
   :update-fn api/update
   :comparison-fn #(= (:name %) "US Doll Hairs")
   :update-params {:name "US Doll Hairs"
                   :symbol "USD"
                   :type "currency"}})

(deftest-delete delete-a-commodity
  commodities-context
  {:resource-name "commodity"
   :storage storage-spec
   :find-resource-fn #(find-commodity % "USD")
   :find-user-fn #(find-user % "john@doe.com")
   :find-other-user-fn #(find-user % "jane@doe.com")
   :delete-fn api/delete
   :select-resources-fn #(commodities/search
                           storage-spec
                           {:entity-id (:id (find-entity % "Personal"))}) })
