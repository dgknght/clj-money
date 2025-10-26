(ns clj-money.entities.attachments-test
  (:require [clojure.test :refer [is]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.images.sql]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :refer [put-and-propagate
                                                  delete-and-propagate]]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.entity-helpers :as helpers :refer [assert-deleted
                                                         assert-updated
                                                         assert-invalid]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-transaction
                                            find-image
                                            find-attachment]]
            [clj-money.test-helpers :refer [dbtest]]))

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
                :content "resources/fixtures/sample_receipt.jpg"
                :content-type "image/jpeg"}))

(defn- attributes []
  #:attachment{:transaction (find-transaction [(t/local-date 2017 1 1) "Paycheck"])
               :image (find-image "sample_receipt.jpg")
               :caption "receipt"})

(defn- assert-created
  [attrs]
  (helpers/assert-created attrs :refs [:attachment/transaction
                                       :attachment/image]))

(dbtest create-an-attachment
  (with-context attach-context
    (assert-created (attributes))))

(dbtest propagate-attachment-creation
  (with-context attach-context
    (put-and-propagate (attributes))
    (is (comparable? {:transaction/attachment-count 1}
                     (entities/find-by #:transaction{:transaction-date (t/local-date 2017 1 1)
                                                   :description "Paycheck"}))
        "The attachment count is incremented for the associated transaction")))

(dbtest transaction-is-required
  (with-context attach-context
    (assert-invalid (-> (attributes)
                        (dissoc :attachment/transaction)
                        (assoc :attachment/transaction-date (t/local-date 2017 1 1)))
                    {:attachment/transaction ["Transaction is required"]})))

(dbtest image-is-required
  (with-context attach-context
    (assert-invalid (dissoc (attributes) :attachment/image)
                    {:attachment/image ["Image is required"]})))

(def ^:private update-context
  (conj attach-context 
        #:attachment{:transaction [(t/local-date 2017 1 1) "Paycheck"] 
                     :image "sample_receipt.jpg"
                     :caption "receipt"}))

(dbtest update-an-attachment
  (with-context update-context
    (assert-updated (find-attachment "receipt")
                    {:attachment/caption "Updated caption"})))

(dbtest delete-an-attachment
  (with-context update-context
    (assert-deleted (find-attachment "receipt"))))

(dbtest propagate-attachment-deletion
  (with-context update-context
    (let [att (find-attachment "receipt")
          trx (entities/find-by #:transaction{:transaction-date (t/local-date 2017 1 1)
                                            :description "Paycheck"})]
      (is (comparable? {:transaction/attachment-count 1}
                       trx)
          "The count reflects the attachment before delete")

      (delete-and-propagate att)

      (is (comparable? {:transaction/attachment-count 0}
                       (entities/find trx))
          "The attachment count is decremented for the associated transaction"))))
