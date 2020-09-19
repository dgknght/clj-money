(ns clj-money.models.attachments-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.validation :as validation]
            [clj-money.models.attachments :as attachments]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.test-context :refer [realize
                                            find-attachment]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

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
  (let [context (realize attach-context)
        result (attachments/create (attributes context))
        transaction (-> context :transactions first)
        retrieved (first
                    (attachments/search {:transaction-id (:id transaction)
                                         :transaction-date (:transaction-date transaction)}))]
    (is (empty? (validation/error-messages result))
        "The attachment is saved successfully")
    (is retrieved "The value can be retreived from the database")
    (is (= "receipt" (:caption retrieved)) "The caption is retrieved correctly")))

(deftest transaction-id-is-required
  (let [context (realize attach-context)
        result (attachments/create (dissoc (attributes context)
                                           :transaction-id))]
    (is (not (validation/valid? result))
        "The value can be retreived from the database")
    (is (seq (validation/error-messages result :transaction-id))
        "The transaction-id attribute has an error message")))

(deftest image-id-is-required
  (let [context (realize attach-context)
        result (attachments/create (dissoc (attributes context)
                                           :image-id))]
    (is (not (validation/valid? result))
        "The value can be retreived from the database")
    (is (seq (validation/error-messages result :image-id))
        "The image-id attribute has an error message")))

(def ^:private update-context
  (assoc attach-context :attachments
                        [{:transaction-id {:transaction-date (t/local-date 2017 1 1)
                                           :description "Paycheck"}
                          :image-id "sample_receipt.jpg"
                          :caption "receipt"}]))

(deftest update-an-attachment
  (let [ctx (realize update-context)
        attachment (find-attachment ctx "receipt")
        result (attachments/update (assoc attachment
                                          :caption "Updated caption"))
        retrieved (attachments/find attachment)]
    (is (empty? (validation/error-messages result)) "There are no validation errors")
    (is (= "Updated caption" (:caption result)) "The updated value is returned")
    (is (= "Updated caption" (:caption retrieved)) "The correct value is retrieved")))

(deftest delete-an-attachment
  (let [context (realize update-context)
        attachment (-> context :attachments first)
        _ (attachments/delete attachment)
        retrieved (attachments/find attachment)]
    (is (nil? retrieved) "The value cannot be retrieved after delete")))
