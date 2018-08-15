(ns clj-money.api.accounts-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.api.test-helper :refer [deftest-create
                                               deftest-delete
                                               deftest-update
                                               deftest-list]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :as h]
            [clj-money.api.accounts :as api]
            [clj-money.models.accounts :as accounts]))

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
                  :type :currency}]
   :accounts [{:name "Checking"
               :type :asset}]})

(defn- find-user        [ctx] (h/find-user ctx "john@doe.com"))
(defn- find-other-user  [ctx] (h/find-user ctx "jane@doe.com"))
(defn- find-entity      [ctx] (h/find-entity ctx "Personal"))
(defn- find-resource    [ctx] (h/find-account ctx "Checking"))
(defn- select-resources [ctx]
  (accounts/search storage-spec {:entity-id (:id (find-entity ctx))}))

(deftest-create create-an-account
  {:resource-name "account"
   :create-fn api/create
   :create-params-fn (fn [ctx]
                       {:entity-id (:id (find-entity ctx))
                        :name "Savings"
                        :type "asset"})
   :compare-fn #(= (:name %) "Savings")})

(deftest-update update-a-account
  {:resource-name "account"
   :find-updated-resource-fn #(accounts/find-by-id storage-spec (:id %))
   :update-fn api/update
   :comparison-fn #(= (:name %) "Bag o' Money")
   :update-params {:name "Bag o' Money"
                   :type "asset"}})

(deftest-delete delete-a-account
  {:resource-name "account"
   :delete-fn api/delete})
