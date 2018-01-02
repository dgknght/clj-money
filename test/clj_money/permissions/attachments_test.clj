(ns clj-money.permissions.attachments-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [slingshot.test :refer :all]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.authorization :refer [apply-scope
                                             allowed?
                                             tag-resource]]
            [clj-money.models.attachments :as attachments]
            [clj-money.models.grants :as grants]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.attachments]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-users
                                            find-transaction
                                            find-image
                                            find-attachment]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private attachments-context
  {:users [(factory :user, {:email "john@doe.com"})
           (factory :user, {:email "jane@doe.com"})]
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
                            :amount 1000M}
                           {:action :credit
                            :account-id "Salary"
                            :amount 1000M}]}]
   :images [{:original-filename "sample_receipt.jpg"
             :body "resources/fixtures/sample_receipt.jpg"
             :content-type "image/jpeg"}]
   :attachments [{:image-id "sample_receipt.jpg"
                  :transaction-id {:transaction-date (t/local-date 2017 1 1)
                                   :description "Paycheck"}
                  :caption "receipt"}]})

;TODO need to figure out how to handle this
#_(deftest attachment-list
  (let [context (serialization/realize storage-spec attachments-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        transaction (find-transaction context (t/local-date 2017 1 1) "Paycheck")]
    (testing "A user has permission to list attachments in his entities"
      (with-authentication john
        (is (not= 0 (->> (apply-scope {:transaction-id (:id transaction)} :attachment)
                         (attachments/search storage-spec)
                         count))
            "The attachments are returned")))
    (testing "A user does not have permission list attachments in someone else's attachment"
      (with-authentication jane
        (is (thrown+? [:type :clj-money.authorization/unauthorized]
                      (->> (apply-scope {:transaction-id (:id transaction)} :attachment)
                         (attachments/search storage-spec)
                         count)))))))

(deftest attachment-management
  (let [context (serialization/realize storage-spec attachments-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        attachment (find-attachment context "receipt")
        transaction (transactions/find-by-id storage-spec
                                             (:transaction-id attachment)
                                             (:transaction-date attachment))]
    (testing "A user has permissions on attachment in his own entities"
      (with-authentication john
        (doseq [action [:show :edit :update :delete]]
          (is (allowed? action attachment)
              (format "A user has %s permission" action)))))
    (testing "A user does not have permissions on attachment in someone else's entities"
      (with-authentication jane
        (doseq [action [:show :edit :update :delete]]
          (is (not (allowed? action attachment))
              (format "A user does not have  %s permission" action)))))
    (testing "A user can be granted permissions on attachment in someone else's entities"
      (grants/create storage-spec {:user-id (:id jane)
                                   :entity-id (:entity-id transaction)
                                   :permissions {:attachment #{:show}}})
      (with-authentication jane
        (doseq [action [:show]]
          (is (allowed? action attachment)
              (format "A user has %s permission" action)))
        (doseq [action [:edit :update :delete]]
          (is (not (allowed? action attachment))
              (format "A user does not have  %s permission" action)))))))

(deftest attachment-creation
  (let [context (serialization/realize storage-spec attachments-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        transaction (find-transaction context (t/local-date 2017 1 1) "Paycheck")
        image (find-image context "sample_receipt.jpg")
        attachment (tag-resource {:image-id (:id image)
                                  :caption "other receipt"
                                  :transaction-id (:id transaction)
                                  :transaction-date (:transaction-date transaction)}
                                 :attachment)]
    (testing "A user has permission to create a attachments in his own entities"
      (with-authentication john
        (is (allowed? :create attachment)
            "Create is allowed")))
    (testing "A user does not have permission to create a attachment in someone else's entities"
      (with-authentication jane
        (is (not (allowed? :create attachment))
            "Create is not allowed")))))
