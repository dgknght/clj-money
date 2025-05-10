(ns clj-money.models.attachments-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.models :as models]
            [clj-money.models.propagation :refer [put-and-propagate
                                                  delete-and-propagate]]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.model-helpers :as helpers :refer [assert-deleted
                                                         assert-invalid]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-transaction
                                            find-image
                                            find-attachment]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def ^:private attach-context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :credit-account "Checking"
                      :debit-account "Salary"
                      :quantity 1000M }
        #:image{:user "john@doe.com"
                :original-filename "sample_receipt.jpg"
                :body "resources/fixtures/sample_receipt.jpg"
                :content-type "image/jpeg"}))

(defn- attributes []
  #:attachment{:transaction (find-transaction [(t/local-date 2017 1 1) "Paycheck"])
               :image (find-image "sample_receipt.jpg")
               :caption "receipt"})

(defn- assert-created
  [attrs]
  (helpers/assert-created attrs :refs [:attachment/transaction
                                       :attachment/image]))

(deftest create-an-attachment
  (with-context attach-context
    (assert-created (attributes))))

(deftest propagate-attachment-creation
  (with-context attach-context
    (put-and-propagate (attributes))
    (is (comparable? {:transaction/attachment-count 1}
                     (models/find-by #:transaction{:transaction-date (t/local-date 2017 1 1)
                                                   :description "Paycheck"}))
        "The attachment count is incremented for the associated transaction")))

(deftest transaction-is-required
  (with-context attach-context
    (assert-invalid (-> (attributes)
                        (dissoc :attachment/transaction)
                        (assoc :attachment/transaction-date (t/local-date 2017 1 1)))
                    {:attachment/transaction ["Transaction is required"]})))

(deftest image-is-required
  (with-context attach-context
    (assert-invalid (dissoc (attributes) :attachment/image)
                    {:attachment/image ["Image is required"]})))

(def ^:private update-context
  (conj attach-context 
        #:attachment{:transaction [(t/local-date 2017 1 1) "Paycheck"] 
                     :image "sample_receipt.jpg"
                     :caption "receipt"}))

(deftest update-an-attachment
  (with-context update-context
    (let [att (find-attachment "receipt")]
      (is (comparable? {:attachment/caption "Updated caption"}
                       (-> att
                           (assoc :attachment/caption "Updated caption")
                           models/put))
          "The return value has the updated attributes")
      (is (comparable? {:attachment/caption "Updated caption"}
                       (models/find att))
          "The retrieved valud has the updated attributes"))))

(deftest delete-an-attachment
  (with-context update-context
    (assert-deleted (find-attachment "receipt"))))

(deftest propagate-attachment-deletion
  (with-context update-context
    (let [att (find-attachment "receipt")
          trx (models/find-by #:transaction{:transaction-date (t/local-date 2017 1 1)
                                            :description "Paycheck"})]
      (is (comparable? {:transaction/attachment-count 1}
                       trx)
          "The count reflects the attachment before delete")

      (delete-and-propagate att)

      (is (comparable? {:transaction/attachment-count 0}
                       (models/find trx))
          "The attachment count is decremented for the associated transaction"))))
