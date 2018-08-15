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
            [clj-money.test-helpers :as h]
            [clj-money.api.commodities :as api]
            [clj-money.models.commodities :as commodities])
  (:import clojure.lang.ExceptionInfo))

(def storage-spec (env :db))

(use-fixtures :each (partial h/reset-db storage-spec))

(def ^:private context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]})

(defn- find-user        [ctx] (h/find-user ctx "john@doe.com"))
(defn- find-other-user  [ctx] (h/find-user ctx "jane@doe.com"))
(defn- find-entity      [ctx] (h/find-entity ctx "Personal"))
(defn- find-resource    [ctx] (h/find-commodity ctx "USD"))
(defn- select-resources [ctx]
  (commodities/search storage-spec {:entity-id (:id (find-entity ctx))}))

(def ^:private commodity-attributes
  {:resource-name "commodity"
   :type "stock"
   :name "Apple, Inc."
   :symbol "AAPL"
   :exchange "nasdaq"})

(deftest-create create-a-commodity
  {:resource-name "commodity"
   :create-fn api/create
   :create-params-fn #(assoc commodity-attributes :entity-id (:id (find-entity %)))
   :compare-fn #(= (:symbol %) "AAPL")})

(deftest-update update-a-commodity
  {:resource-name "commodity"
   :find-updated-resource-fn #(commodities/find-by-id storage-spec (:id %))
   :update-fn api/update
   :comparison-fn #(= (:name %) "US Doll Hairs")
   :update-params {:name "US Doll Hairs"
                   :symbol "USD"
                   :type "currency"}})

(deftest-delete delete-a-commodity
  {:resource-name "commodity"
   :delete-fn api/delete})
