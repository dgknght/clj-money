(ns clj-money.models.lots-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.validation :as validation]
            [clj-money.models.lots :as lots]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private lot-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]
   :commodities [{:name "Apple"
                  :symbol "APPL"
                  :exchange :nasdaq}]})

(deftest create-a-lot
  (let [context (serialization/realize storage-spec lot-context)
        commodity (-> context :commodities first)
        result (lots/create storage-spec {:commodity-id (:id commodity)
                                          :purchase-date (t/local-date 2017 3 2)
                                          :shares 100M})
        lots (lots/select-by-commodity-id storage-spec (:id commodity))]
    (is (:id result) "The result receives an ID value")
    (is (empty? (validation/error-messages result)) "The result contains no validation errors")
    (is (= [{:purchase-date (t/local-date 2017 3 2)
             :shares 100M}]
           (map #(select-keys % [:purchase-date :shares]) lots))
        "The value is retrieved after create")))
