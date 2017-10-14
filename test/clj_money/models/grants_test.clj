(ns clj-money.models.grants-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.tagging :as tagging]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.test-helpers :refer [reset-db
                                            find-entity
                                            find-user
                                            find-grant]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private grant-context
  {:users (map #(factory :user {:email %})
                ["john@doe.com" "jane@doe.com"])
   :entities [{:name "Business"
               :user-id "john@doe.com"}]
   :commodities [{:name "US Dollar"
                  :type :currency
                  :symbol "USD"}]})

(deftest create-an-entity
  (let [context (serialization/realize storage-spec grant-context)
        entity (find-entity context "Business")
        user (find-user context "jane@doe.com")
        grant {:entity-id (:id entity)
               :user-id (:id user)
               :permissions {:account [:index :show]}}
        result (grants/create storage-spec grant)
        grant-list (map #(dissoc % :updated-at :created-at :id)
                        (grants/search storage-spec {:entity-id (:id entity)}))
        expected-grant-list [{:entity-id (:id entity)
                              :user-id (:id user)
                              :permissions {:account [:index :show]}}]]
    (is (empty? (validation/error-messages result))
        "The result does not contain any validation errors")

    (when-not (= expected-grant-list grant-list)
      (pprint {:expected expected-grant-list
               :actual grant-list
               :diff (diff expected-grant-list grant-list)}))
    (is (= expected-grant-list
           grant-list)
        "The grant exists in the list after create")))

(def ^:private existing-grant-context
  (assoc grant-context :grants [{:user-id "jane@doe.com"
                                 :entity-id "Business"
                                 :permissions {:account [:index :show]}}]))

(deftest update-a-grant
  (let [context (serialization/realize storage-spec existing-grant-context)
        entity (find-entity context "Business")
        user (find-user context "jane@doe.com")
        grant (find-grant context (:id entity) (:id user))
        updated (update-in grant
                           [:permissions]
                           #(assoc % :transactions [:index :show]))
        result (grants/update storage-spec updated)
        retrieved (grants/find-by-id storage-spec (:id result))]
    (is (empty? (validation/error-messages result))
        "The result has not validation errors")
    (is (= {:account [:index :show]
            :transactions [:index :show]}
           (:permissions retrieved))
        "The retrieved record should have the correct content")))

(deftest delete-a-grant
  (let [context (serialization/realize storage-spec existing-grant-context)
        entity (find-entity context "Business")
        user (find-user context "jane@doe.com")
        grant (find-grant context (:id entity) (:id user))
        _ (grants/delete storage-spec (:id grant))
        grant-list (grants/search storage-spec {:entity-id (:id entity)})]
    (is (empty? (filter #(= (:id grant) (:id %)) grant-list))
        "The grant is not present after delete")))
