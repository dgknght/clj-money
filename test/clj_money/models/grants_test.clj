(ns clj-money.models.grants-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-entity
                                            find-user
                                            find-grant]]
            [clj-money.models.grants :as grants]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def ^:private grant-context
  {:users (map #(factory :user {:email %})
               ["john@doe.com" "jane@doe.com"])
   :entities [{:name "Business"
               :user-id "john@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}]})

(deftest create-a-grant
  (let [context (realize grant-context)
        entity (find-entity context "Business")
        user (find-user context "jane@doe.com")
        grant {:entity-id (:id entity)
               :user-id (:id user)
               :permissions {:account #{:index :show}}}
        result (grants/create grant)
        retrieved (grants/find result)]
    (is (valid? result)) 
    (is (comparable? grant result) "The return value has correct attributes")
    (is (comparable? grant retrieved)
        "The retrieved value has the correct attributes")))

(def ^:private existing-grant-context
  (assoc grant-context :grants [{:user-id "jane@doe.com"
                                 :entity-id "Business"
                                 :permissions {:account #{:index :show}}}]))

(deftest update-a-grant
  (let [context (realize existing-grant-context)
        entity (find-entity context "Business")
        user (find-user context "jane@doe.com")
        grant (find-grant context (:id entity) (:id user))
        result (grants/update
                (update-in grant
                           [:permissions]
                           #(assoc % :transaction #{:index :show})))
        retrieved (grants/find result)]
    (is (valid? result))
    (is (= {:account #{:index :show}
            :transaction #{:index :show}}
           (:permissions retrieved))
        "The retrieved record should have the correct content")))

(deftest delete-a-grant
  (let [context (realize existing-grant-context)
        entity (find-entity context "Business")
        user (find-user context "jane@doe.com")
        grant (find-grant context (:id entity) (:id user))
        _ (grants/delete grant)
        grant-list (grants/search {:entity-id (:id entity)})]
    (is (empty? (filter #(= (:id grant) (:id %)) grant-list))
        "The grant is not present after delete")))
