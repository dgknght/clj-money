(ns clj-money.api.receipt-ingestions-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.util :as util]
            [clj-money.images :as images]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.email :as email]
            [clj-money.ingestion.helpers :as hlp]
            [clj-money.test-helpers :refer [reset-db
                                            propagated]]
            [clj-money.api.util :refer [+query]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               request
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-accounts
                                            find-transaction
                                            find-receipt-ingestion]]
            [clj-money.entities :as entities]
            [clj-money.web.test-handler :refer [build-app]]))

(use-fixtures :each reset-db)

(def ^:private extracted-receipt
  {:date "2015-01-02"
   :location-name "Kroger"
   :total 10.5M
   :tax 0M
   :tax-rate 0M
   :payment-account "Checking"
   :line-items [{:description "Milk"
                 :amount 10.5M
                 :account "Groceries"}]})

(defn- reader
  "Returns a reader that returns the result of calling f"
  [f]
  (reify
    ing/ReceiptReader
    (read-receipt [_ _source _entity _opts] (f))
    ing/Closeable
    (close [_])))

(def ^:private successful-reader
  (reader (constantly extracted-receipt)))

(def ^:private failing-reader
  (reader #(throw (ex-info "The model is unavailable" {}))))

(defn- await-completion
  "Waits for the background read of the receipt-ingestion to finish, and for
  the changes it made (e.g., the transaction's attachment count) to be
  propagated, returning the receipt-ingestion as it was last retrieved."
  [{:keys [id]}]
  (loop [n 0]
    (let [ingestion (entities/find id)]
      (if (or (#{:complete :failed} (:receipt-ingestion/status ingestion))
              (<= 50 n))
        (propagated ingestion)
        (do (Thread/sleep 100)
            (recur (inc n)))))))

(defn- create-ingestion
  [email & {:keys [reader file content-type]
            :or {reader successful-reader
                 file "fixtures/attachment.jpg"
                 content-type "image/jpeg"}}]
  (let [entity (find-entity "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :receipt-ingestions))
                     (merge (build-multipart-request {:image {:file (io/file file)
                                                              :content-type content-type}}))
                     (add-auth (find-user email))
                     (req/header "Accept" "application/edn")
                     ((build-app {:receipt-reader reader}))
                     parse-body)]
    [response
     (some-> response
             :parsed-body
             await-completion)]))

(defn- configure-ingestion! []
  (let [entity (find-entity "Personal")
        [checking rent] (find-accounts "Checking" "Rent")]
    (entities/put (-> entity
                      (assoc-in [:entity/settings
                                 :settings/expense-accounts]
                                #{(util/->entity-ref rent)})
                      (assoc-in [:entity/settings
                                 :settings/payment-methods]
                                #{(util/->entity-ref checking)})))))

(deftest a-user-can-create-a-receipt-ingestion-in-his-entity
  (with-context
    (configure-ingestion!)
    (let [[{:as response :keys [parsed-body]} retrieved] (create-ingestion "john@doe.com")]
      (is (http-created? response))
      (is (:id parsed-body) "An ID is assigned to the new record")
      (is (comparable? {:receipt-ingestion/entity (util/->entity-ref (find-entity "Personal"))
                        :receipt-ingestion/status :pending}
                       parsed-body)
          "The response is the new receipt-ingestion, waiting to be read")
      (is (:receipt-ingestion/image retrieved)
          "The receipt image is saved")
      (is (comparable?
            #:receipt-ingestion{:status :complete
                                :receipt {:date "2015-01-02"
                                          :location-name "Kroger"
                                          :payment-account "Checking"
                                          :line-items [{:account "Groceries"
                                                        :amount 10.5M}]}}
            retrieved)
          "The receipt details are saved")
      (is (comparable? #:transaction{:transaction-date (t/local-date 2015 1 2)
                                     :description "Kroger"
                                     :value 10.5M
                                     :source :ingestion
                                     :review-status :pending}
                       (some-> retrieved :receipt-ingestion/transaction entities/find))
          "A transaction awaiting review is created from the receipt")
      (let [trx (some-> retrieved :receipt-ingestion/transaction entities/find)]
        (is (= 1 (:transaction/attachment-count trx))
            "The transaction's attachment count is updated")
        (is (seq-of-maps-like? [#:attachment{:image (:receipt-ingestion/image retrieved)
                                             :caption "Receipt"}]
                               (entities/select #:attachment{:transaction trx}))
            "The receipt image is attached to the transaction")
        (is (util/id= trx
                      (:receipt-ingestion/transaction retrieved))
            "The receipt-ingestion refers to the transaction")))))

(deftest a-user-can-create-a-receipt-ingestion-from-an-email
  (with-context
    (configure-ingestion!)
    (let [inner (hlp/inspectable-reader :result extracted-receipt)
          [response retrieved] (create-ingestion
                                 "john@doe.com"
                                 :file "fixtures/emails/plain.eml"
                                 :content-type "message/rfc822"
                                 :reader (ing/receipt-reader {::ing/provider ::email/email
                                                              :reader inner}))
          [[contents]] (:read-receipt (hlp/calls inner))]
      (is (http-created? response))
      (is (= :email (:origin contents))
          "The text of the email is read")
      (is (comparable? #:receipt-ingestion{:status :complete}
                       retrieved)
          "The receipt is read")
      (let [trx (some-> retrieved :receipt-ingestion/transaction entities/find)]
        (is (comparable? #:transaction{:description "Kroger"
                                       :review-status :pending}
                         trx)
            "A transaction awaiting review is created from the email")
        (is (seq-of-maps-like? [#:attachment{:caption "Email"
                                             :image #:image{:content-type "application/pdf"
                                                            :original-filename "email.pdf"}}
                                #:attachment{:caption "Receipt"
                                             :image #:image{:content-type "message/rfc822"}}]
                               (->> (entities/select #:attachment{:transaction trx})
                                    (map #(update-in % [:attachment/image] entities/find))
                                    (sort-by :attachment/caption)))
            "The email is attached to the transaction, with a PDF of its body")))))

(deftest the-files-attached-to-an-email-are-attached-to-the-transaction
  (with-context
    (configure-ingestion!)
    (let [[_ retrieved] (create-ingestion
                          "john@doe.com"
                          :file "fixtures/emails/embedded_image.eml"
                          :content-type "message/rfc822"
                          :reader (ing/receipt-reader {::ing/provider ::email/email
                                                       :reader (hlp/inspectable-reader
                                                                 :result extracted-receipt)}))
          trx (some-> retrieved :receipt-ingestion/transaction entities/find)
          attachments (->> (entities/select #:attachment{:transaction trx})
                           (map #(update-in % [:attachment/image] entities/find))
                           (sort-by :attachment/caption))]
      (is (= 2 (:transaction/attachment-count trx))
          "The transaction's attachment count includes the extracted file")
      (is (seq-of-maps-like? [#:attachment{:caption "Receipt"
                                           :image #:image{:content-type "message/rfc822"}}
                              #:attachment{:caption "invoice.pdf"
                                           :image #:image{:content-type "application/pdf"
                                                          :original-filename "invoice.pdf"}}]
                             attachments)
          "The PDF attached to the email is attached to the transaction beside the email, and the logo isn't")
      (is (= "%PDF-1.4 fake invoice"
             (some-> (second attachments)
                     (get-in [:attachment/image :image/uuid])
                     images/get
                     (String. "US-ASCII")))
          "The PDF's content is stored"))))

(deftest an-email-that-isnt-a-receipt-is-recorded
  (with-context
    (configure-ingestion!)
    (let [[response retrieved] (create-ingestion
                                 "john@doe.com"
                                 :reader (reader (constantly {:is-receipt false})))]
      (is (http-created? response))
      (is (comparable? #:receipt-ingestion{:status :failed
                                           :failure-reason :not-a-receipt
                                           :error "That isn't a receipt."
                                           :receipt {:is-receipt false}}
                       retrieved)
          "The read fails, recording that it isn't a receipt")
      (is (nil? (:receipt-ingestion/transaction retrieved))
          "No transaction is created")
      (is (empty? (entities/select #:transaction{:entity (find-entity "Personal")
                                                 :source :ingestion}))
          "No transaction is saved"))))

(deftest a-failed-read-is-recorded
  (with-context
    (let [[response retrieved] (create-ingestion "john@doe.com"
                                                 :reader failing-reader)]
      (is (http-created? response))
      (is (comparable? #:receipt-ingestion{:status :failed
                                           :error "The model is unavailable"}
                       retrieved)
          "The error is recorded on the receipt-ingestion"))))

(deftest a-user-cannot-create-a-receipt-ingestion-in-anothers-entity
  (with-context
    (let [[response] (create-ingestion "jane@doe.com")]
      (is (http-not-found? response))
      (is (empty? (entities/select {} :entity-type :receipt-ingestion))
          "No receipt-ingestion is created"))))

(def ^:private ingestion-context
  (conj basic-context
        #:image{:user "john@doe.com"
                :original-filename "receipt.jpg"
                :content-type "image/jpeg"
                :content (io/file "fixtures/attachment.jpg")}
        #:transaction{:description "Kroger"
                      :entity "Personal"
                      :transaction-date (t/local-date 2015 1 2)
                      :quantity 10.5M
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :source :ingestion
                      :review-status :pending}))

(def ^:private completed-ingestion-context
  (conj ingestion-context
        #:receipt-ingestion{:entity "Personal"
                            :image "receipt.jpg"
                            :status :complete
                            :transaction [(t/local-date 2015 1 2) "Kroger"]}))

(def ^:private pending-ingestion-context
  (conj ingestion-context
        #:receipt-ingestion{:entity "Personal"
                            :image "receipt.jpg"
                            :status :pending
                            :transaction [(t/local-date 2015 1 2) "Kroger"]}))

(defn- get-ingestion
  [email]
  (-> (request :get (path :api
                          :receipt-ingestions
                          (:id (find-receipt-ingestion "Personal")))
               :user (find-user email))
      ((build-app {:receipt-reader successful-reader}))
      parse-body))

(deftest a-user-can-get-a-receipt-ingestion-in-his-entity
  (with-context completed-ingestion-context
    (let [{:as response :keys [parsed-body]} (get-ingestion "john@doe.com")]
      (is (http-success? response))
      (is (comparable? #:receipt-ingestion{:status :complete}
                       parsed-body)
          "The receipt-ingestion is returned"))))

(deftest a-user-cannot-get-a-receipt-ingestion-in-anothers-entity
  (with-context completed-ingestion-context
    (is (http-not-found? (get-ingestion "jane@doe.com")))))

(defn- get-ingestions-for-transactions
  [email & {:keys [query]}]
  (-> (request :get (+query (path :api
                                  :entities
                                  (:id (find-entity "Personal"))
                                  :receipt-ingestions)
                            (or query
                                {:transaction-id (:id (find-transaction [(t/local-date 2015 1 2) "Kroger"]))}))
               :user (find-user email))
      ((build-app {:receipt-reader successful-reader}))
      parse-body))

(deftest a-user-can-get-the-receipt-ingestions-for-transactions-in-his-entity
  (with-context completed-ingestion-context
    (let [{:as response :keys [parsed-body]} (get-ingestions-for-transactions "john@doe.com")]
      (is (http-success? response))
      (is (seq-of-maps-like? [#:receipt-ingestion{:status :complete
                                                   :transaction (util/->entity-ref
                                                                  (find-transaction [(t/local-date 2015 1 2) "Kroger"]))}]
                             parsed-body)
          "The receipt-ingestion that created the transaction is returned"))))

(deftest a-transaction-is-required-to-get-receipt-ingestions
  (with-context completed-ingestion-context
    (is (http-bad-request? (get-ingestions-for-transactions "john@doe.com" :query {})))))

(deftest a-user-cannot-get-the-receipt-ingestions-for-transactions-in-anothers-entity
  (with-context completed-ingestion-context
    (let [{:as response :keys [parsed-body]} (get-ingestions-for-transactions "jane@doe.com")]
      (is (http-success? response))
      (is (empty? parsed-body) "No receipt-ingestions are returned"))))

(defn- reject-ingestion
  [email & {:keys [reason] :or {reason "Wrong store"}}]
  (let [ingestion (find-receipt-ingestion "Personal")
        transaction (find-transaction [(t/local-date 2015 1 2) "Kroger"])
        response (-> (request :patch (path :api
                                           :receipt-ingestions
                                           (:id ingestion))
                              :user (find-user email)
                              :body #:receipt-ingestion{:status :rejected
                                                        :rejection-reason reason})
                     ((build-app {:receipt-reader successful-reader}))
                     parse-body)]
    [response
     (entities/find ingestion)
     (entities/find transaction)]))

(deftest a-user-can-reject-an-ingested-transaction-in-his-entity
  (with-context completed-ingestion-context
    (let [[response ingestion transaction] (reject-ingestion "john@doe.com")]
      (is (http-success? response))
      (is (comparable? #:receipt-ingestion{:status :rejected
                                           :rejection-reason "Wrong store"}
                       ingestion)
          "The rejection is recorded on the receipt-ingestion")
      (is (nil? (:receipt-ingestion/transaction ingestion))
          "The receipt-ingestion no longer refers to the transaction")
      (is (nil? transaction)
          "The transaction is deleted"))))

(deftest a-user-cannot-reject-a-pending-ingested-transaction-in-his-entity
  (with-context pending-ingestion-context
    (let [[response ingestion transaction] (reject-ingestion "john@doe.com"
                                                             :reason "Wrong store")]
      (is (http-bad-request? response))
      (is (comparable? #:receipt-ingestion{:status :pending}
                       ingestion)
          "The receipt-ingestion is not updated")
      (is transaction "The transaction is not deleted"))))

(deftest a-reason-is-required-to-reject-an-ingested-transaction
  (with-context completed-ingestion-context
    (let [[response ingestion transaction] (reject-ingestion "john@doe.com"
                                                             :reason "")]
      (is (http-bad-request? response))
      (is (comparable? #:receipt-ingestion{:status :complete}
                       ingestion)
          "The receipt-ingestion is not updated")
      (is transaction "The transaction is not deleted"))))

(deftest a-user-cannot-reject-an-ingested-transaction-in-anothers-entity
  (with-context completed-ingestion-context
    (let [[response ingestion transaction] (reject-ingestion "jane@doe.com")]
      (is (http-not-found? response))
      (is (comparable? #:receipt-ingestion{:status :complete}
                       ingestion)
          "The receipt-ingestion is not updated")
      (is transaction "The transaction is not deleted"))))
