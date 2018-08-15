(ns clj-money.api.entities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [clj-money.api.test-helper :refer [deftest-list
                                               deftest-create
                                               deftest-delete
                                               deftest-update]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :as h]
            [clj-money.api.entities :as api]
            [clj-money.models.entities :as entities]))

(def storage-spec (env :db))

(use-fixtures :each (partial h/reset-db storage-spec))

(defn- find-user        [ctx] (h/find-user ctx "john@doe.com"))
(defn- find-other-user  [ctx] (h/find-user ctx "jane@doe.com"))
(defn- find-entity      [ctx] (h/find-entity ctx "Personal"))
(defn- find-resource    [ctx] (h/find-entity ctx "Personal"))
(defn- select-resources [ctx] (entities/select storage-spec (:id (find-user ctx))))

(def ^:private context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]
   :entities [{:user-id "john@doe.com"
               :name "Personal"}]})

(deftest-create create-an-entity
  {:resource-name "entity"
   :create-fn api/create
   :create-params-fn (fn [_]
                       {:name "Business"
                        :settings {:inventory-method "fifo"}})
   :compare-fn #(= (:name %) "Business")
   :skip-auth-failure-test true})

(deftest-update update-an-entity
  {:resource-name "entity"
   :find-updated-resource-fn #(entities/find-by-id storage-spec (:id %))
   :update-fn api/update
   :comparison-fn #(= (:name %) "My Stuff")
   :update-params {:name "My Stuff"
                   :settings {:inventory-method "fifo"}}})

(deftest-delete delete-an-entity
  {:resource-name "entity"
   :delete-fn api/delete })
