(ns clj-money.models.imports-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-imports]]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff]]
            [clj-money.validation :as validation]
            [clj-money.models.imports :as imports]
            [clj-money.models.images :as images]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def import-context
  {:users [(factory :user {:email "john@doe.com"})]
   :images [{:original-filename "sample.gnucash"
             :content-type "application/gnucash"
             :body "resources/fixtures/sample.gnucash"}]})

(def existing-imports-context
  (assoc import-context :imports [{:user-id "john@doe.com"
                                   :entity-name "import entity"
                                   :image-ids ["sample.gnucash"]}]))

(deftest get-a-list-of-imports
  (let [context (realize storage-spec existing-imports-context)
        user (-> context :users first)
        actual (map #(dissoc % :id :created-at :updated-at)
                    (imports/search storage-spec
                                    {:user-id (:id user)}))
        expected [{:entity-name "import entity"
                   :user-id (:id user)
                   :progress {}
                   :image-ids (map :id (:images context))
                   :entity-exists? false}]]
    (pprint-diff expected actual)
    (is (= expected actual))))

(defn attributes
  [context]
  {:user-id (-> context :users first :id)
   :entity-name "Personal"
   :image-ids (->> (:images context)
                   (map :id)
                   (take 1))})

(deftest create-an-import
  (let [context (realize storage-spec import-context)
        result (imports/create storage-spec (attributes context))]

    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (:id result) "It assigns an ID to the result")))

(deftest user-id-is-required
  (let [context (realize storage-spec import-context)
        result (imports/create storage-spec
                               (dissoc (attributes context) :user-id))]
    (is (seq (validation/error-messages result :user-id))
        "There is a validation error on :user-id")))

(deftest image-ids-is-required
  (let [context (realize storage-spec import-context)
        result (imports/create storage-spec
                               (dissoc (attributes context) :image-ids))]
    (is (seq (validation/error-messages result :image-ids))
        "There is a validation error on :image-ids")))

(deftest entity-name-is-required
  (let [context (realize storage-spec import-context)
        result (imports/create storage-spec
                               (dissoc (attributes context) :entity-name))]
    (is (seq (validation/error-messages result :entity-name))
        "There is a validation error on :entity-name")))

(deftest update-an-import
  (let [context (realize storage-spec import-context)
        imp (imports/create storage-spec (attributes context))
        updated (assoc imp :progress {:account {:total 20
                                                :processed 0}})
        _ (imports/update storage-spec updated)
        retrieved (imports/find-by-id storage-spec (:id imp))]
    (is (= {:account {:total 20
                      :processed 0}}
           (:progress retrieved))
        "The correct value is retrieved after update")))

(def ^:private delete-context
  (-> existing-imports-context
      (update-in [:imports] #(concat % [{:user-id "john@doe.com"
                                         :entity-name "other entity"
                                         :image-ids ["sample_with_commodities.gnucash"]}
                                        {:user-id "john@doe.com"
                                         :entity-name "same entity"
                                         :image-ids ["sample.gnucash"]}]))
      (update-in [:images] #(conj % {:original-filename "sample_with_commodities.gnucash"
                                     :content-type "application/gnucash"
                                     :body "resources/fixtures/sample_with_commodities.gnucash"}))))

(deftest delete-an-import
  (let [context (realize storage-spec delete-context)
        user (find-user context "john@doe.com")
        [import-entity
         same-entity] (find-imports context
                                    "import entity"
                                    "same entity")]
    (testing "deleting an import deletes the associated files"
      (imports/delete storage-spec (:id import-entity))
      (is (empty? (imports/search storage-spec {:user-id (:id user)
                                                :entity-name "import entity"}))
          "The import record is removed")
      (is (empty? (images/search storage-spec {:user-id (:id user)
                                               :original-filename "sample.gnucash"}))
          "The image record is removed also"))
    (testing "deleting an import preserves associated files linked to other imports"
      (imports/delete storage-spec (:id same-entity))
      (is (empty? (imports/search storage-spec {:user-id (:id user)
                                                :entity-name "same entity"}))
          "The import record is removed")
      (is (seq (images/search storage-spec {:user-id (:id user)
                                            :original-filename "sample_with_commodities.gnucash"}))
          "The image record is preserved"))))
