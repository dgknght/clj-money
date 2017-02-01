(ns clj-money.models.commodities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private commodity-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]})

(deftest create-a-commodity
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity {:entity-id entity-id
                   :exchange :nasdaq
                   :name "Apple"
                   :symbol "APPL"}
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")
    (is (= [{:name "Apple"
             :symbol "APPL"
             :exchange :nasdaq}]
           (map #(select-keys % [:name :symbol :exchange]) commodities))
        "The commodity can be retrieved after create")))
