(ns clj-money.models.attachments-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.model-helpers :as helpers :refer [assert-invalid]]
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
  #:attachment{:transaction (find-transaction (t/local-date 2017 1 1) "Paycheck")
               :image (find-image "sample_receipt.jpg")
               :caption "receipt"})

(defn- assert-created
  [attrs]
  (helpers/assert-created attrs {:refs [:attachment/transaction
                                        :attachment/image]}))

(deftest create-an-attachment
  (with-context attach-context
    (assert-created (attributes))
    (is (comparable? {:transaction/attachment-count 1}
                     (models/find (find-transaction (t/local-date 2017 1 1) "Paycheck"))))))

; (deftest transaction-id-is-required
;   (let [context (realize attach-context)
;         result (attachments/create (dissoc (attributes context)
;                                            :transaction-id))]
;     (is (invalid? result [:transaction-id] "Transaction is required"))))
; 
; (deftest image-id-is-required
;   (let [context (realize attach-context)
;         result (attachments/create (dissoc (attributes context)
;                                            :image-id))]
;     (is (invalid? result [:image-id] "Image is required"))))
; 
; (def ^:private update-context
;   (assoc attach-context :attachments
;          [{:transaction-id {:transaction-date (t/local-date 2017 1 1)
;                             :description "Paycheck"}
;            :image-id "sample_receipt.jpg"
;            :caption "receipt"}]))
; 
; (deftest update-an-attachment
;   (let [ctx (realize update-context)
;         attachment (find-attachment ctx "receipt")
;         result (attachments/update (assoc attachment
;                                           :caption "Updated caption"))
;         retrieved (attachments/find attachment)]
;     (is (valid? result))
;     (is (= "Updated caption" (:caption result)) "The updated value is returned")
;     (is (= "Updated caption" (:caption retrieved)) "The correct value is retrieved")))
; 
; (deftest delete-an-attachment
;   (let [context (realize update-context)
;         attachment (-> context :attachments first)
;         _ (attachments/delete attachment)
;         retrieved (attachments/find attachment)
;         retrieved-trans (transactions/find attachment)]
;     (is (nil? retrieved) "The value cannot be retrieved after delete")
;     (is (= 0 (:attachment-count retrieved-trans))
;         "The attachment count is updated in the transaction")))
