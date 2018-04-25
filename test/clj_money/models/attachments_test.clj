(ns clj-money.models.attachments-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.validation :as validation]
            [clj-money.models.attachments :as attachments]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private attach-context
  {:users [(factory :user, {:email "john@doe.com"})]
   :entities [{:name "Personal"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Checking"
               :type :asset }
              {:name "Salary"
               :type :income}
              {:name "Groceries"
               :type :expense}]
   :transactions [{:transaction-date (t/local-date 2017 1 1)
                   :description "Paycheck"
                   :items [{:action :debit
                            :account-id "Checking"
                            :quantity 1000M}
                           {:action :credit
                            :account-id "Salary"
                            :quantity 1000M}]}]
   :images [{:original-filename "sample_receipt.jpg"
             :body "resources/fixtures/sample_receipt.jpg"
             :content-type "image/jpeg"}]})

(defn- attributes
  [{[transaction] :transactions :as context}]
  {:transaction-id (:id transaction)
   :transaction-date (:transaction-date transaction)
   :image-id (-> context :images first :id)
   :caption "receipt"})

(deftest create-an-attachment
  (let [context (serialization/realize storage-spec attach-context)
        result (attachments/create storage-spec (attributes context))
        transaction (-> context :transactions first)
        retrieved (->> {:transaction-id (:id transaction)
                        :transaction-date (:transaction-date transaction)}
                       (attachments/search storage-spec)
                       first)]
    (is (empty? (validation/error-messages result))
        "The attachment is saved successfully")
    (is retrieved "The value can be retreived from the database")
    (is (= "receipt" (:caption retrieved)) "The caption is retrieved correctly")))

(deftest transaction-id-is-required
  (let [context (serialization/realize storage-spec attach-context)
        result (attachments/create storage-spec (dissoc (attributes context)
                                                        :transaction-id))]
    (is (not (validation/valid? result))
        "The value can be retreived from the database")
    (is (not (empty? (validation/error-messages result :transaction-id)))
        "The transaction-id attribute has an error message")))

(deftest image-id-is-required
  (let [context (serialization/realize storage-spec attach-context)
        result (attachments/create storage-spec (dissoc (attributes context)
                                                        :image-id))]
    (is (not (validation/valid? result))
        "The value can be retreived from the database")
    (is (not (empty? (validation/error-messages result :image-id)))
        "The image-id attribute has an error message")))

(deftest caption-is-required
  (let [context (serialization/realize storage-spec attach-context)
        result (attachments/create storage-spec (dissoc (attributes context)
                                                        :caption))]
    (is (not (validation/valid? result))
        "The value can be retreived from the database")
    (is (not (empty? (validation/error-messages result :caption)))
        "The caption attribute has an error message")))

(def ^:private delete-context
  (assoc attach-context :attachments
                        [{:transaction-id {:transaction-date (t/local-date 2017 1 1)
                                           :description "Paycheck"}
                          :image-id "sample_receipt.jpg"
                          :caption "receipt"}]))

(deftest delete-an-attachment
  (let [context (serialization/realize storage-spec delete-context)
        attachment (-> context :attachments first)
        _ (attachments/delete storage-spec attachment)
        retrieved (attachments/find-by-id storage-spec (:id attachment))]
    (is (nil? retrieved) "The value cannot be retrieved after delete")))
