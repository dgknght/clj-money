(ns clj-money.models.imports-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-imports]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.imports :as imports]
            [clj-money.models.images :as images]))

(use-fixtures :each reset-db)

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
  (let [context (realize existing-imports-context)
        user (-> context :users first)
        actual (map #(dissoc % :id :created-at :updated-at)
                    (imports/search {:user-id (:id user)}))
        expected [{:entity-name "import entity"
                   :user-id (:id user)
                   :progress {}
                   :options nil
                   :image-ids (map :id (:images context))
                   :entity-exists? false}]]
    (is (= expected actual))))

(defn attributes
  [context]
  {:user-id (-> context :users first :id)
   :entity-name "Personal"
   :options {:lt-capital-gains-account "Investments/Long-Term Gains"
             :st-capital-gains-account "Investments/Short-Term Gains"
             :lt-capital-loss-account "Long-Term Losses"
             :st-capital-loss-account "Short-Term Losses"}
   :image-ids (->> (:images context)
                   (map :id)
                   (take 1))})

(deftest create-an-import
  (let [context (realize import-context)
        attr (attributes context)
        result (imports/create attr)]

    (is (valid? result))
    (is (:id result) "It assigns an ID to the result")
    (is (comparable? attr
                     result)
        "The attributes are stored and retreived correctly.")))

(deftest user-id-is-required
  (let [context (realize import-context)
        result (imports/create (dissoc (attributes context) :user-id))]
    (is (invalid? result [:user-id] "User is required"))))

(deftest image-ids-is-required
  (let [context (realize import-context)
        result (imports/create (dissoc (attributes context) :image-ids))]
    (is (invalid? result [:image-ids] "Image ids is required"))))

(deftest entity-name-is-required
  (let [context (realize import-context)
        result (imports/create (dissoc (attributes context) :entity-name))]
    (is (invalid? result [:entity-name] "Entity name is required"))))

(deftest update-an-import
  (let [context (realize import-context)
        imp (imports/create (attributes context))
        updated (assoc imp :progress {:account {:total 20
                                                :processed 0}})
        _ (imports/update updated)
        retrieved (imports/find imp)]
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
  (let [context (realize delete-context)
        user (find-user context "john@doe.com")
        [import-entity
         same-entity] (find-imports context
                                    "import entity"
                                    "same entity")]
    (testing "deleting an import deletes the associated files"
      (imports/delete import-entity)
      (is (empty? (imports/search {:user-id (:id user)
                                   :entity-name "import entity"}))
          "The import record is removed")
      (is (empty? (images/search {:user-id (:id user)
                                  :original-filename "sample.gnucash"}))
          "The image record is removed also"))
    (testing "deleting an import preserves associated files linked to other imports"
      (imports/delete same-entity)
      (is (empty? (imports/search {:user-id (:id user)
                                   :entity-name "same entity"}))
          "The import record is removed")
      (is (seq (images/search {:user-id (:id user)
                               :original-filename "sample_with_commodities.gnucash"}))
          "The image record is preserved"))))
