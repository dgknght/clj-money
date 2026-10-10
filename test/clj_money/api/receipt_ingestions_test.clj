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
            [clj-money.ingestion :as ing]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               request
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-account
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
  (reify ing/Reader
    (read-receipt [_ _source _entity _opts] (f))
    (close [_])))

(def ^:private successful-reader
  (reader (constantly extracted-receipt)))

(def ^:private failing-reader
  (reader #(throw (ex-info "The model is unavailable" {}))))

(defn- await-completion
  "Waits for the background read of the receipt-ingestion to finish, returning
  the receipt-ingestion as it was last retrieved."
  [{:keys [id]}]
  (loop [n 0]
    (let [ingestion (entities/find id)]
      (if (or (#{:complete :failed} (:receipt-ingestion/status ingestion))
              (<= 50 n))
        ingestion
        (do (Thread/sleep 100)
            (recur (inc n)))))))

(defn- create-ingestion
  [email & {:keys [reader] :or {reader successful-reader}}]
  (let [entity (find-entity "Personal")
        file (io/file (io/resource "fixtures/attachment.jpg"))
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :receipt-ingestions))
                     (merge (build-multipart-request {:image {:file file
                                                              :content-type "image/jpeg"}}))
                     (add-auth (find-user email))
                     (req/header "Accept" "application/edn")
                     ((build-app {:ingestion reader}))
                     parse-body)]
    [response
     (some-> response
             :parsed-body
             await-completion)]))

(deftest a-user-can-create-a-receipt-ingestion-in-his-entity
  (with-context
    (let [[{:as response :keys [parsed-body]} retrieved] (create-ingestion "john@doe.com")]
      (is (http-created? response))
      (is (:id parsed-body) "An ID is assigned to the new record")
      (is (comparable? {:receipt-ingestion/entity (util/->entity-ref (find-entity "Personal"))
                        :receipt-ingestion/status :pending}
                       parsed-body)
          "The response is the new receipt-ingestion, waiting to be read")
      (is (:receipt-ingestion/image retrieved)
          "The receipt image is saved")
      (is (comparable? #:receipt-ingestion{:status :complete
                                           :receipt #:receipt{:transaction-date (t/local-date 2015 1 2)
                                                              :description "Kroger"
                                                              :payment-account (util/->entity-ref (find-account "Checking"))
                                                              :items [#:receipt-item{:account (util/->entity-ref (find-account "Groceries"))
                                                                                     :quantity 10.5M}]}}
                       retrieved)
          "The receipt is read in the background")
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
            "The receipt image is attached to the transaction"))
      (is (= (:receipt/transaction-id (:receipt-ingestion/receipt retrieved))
             (:id (:receipt-ingestion/transaction retrieved)))
          "The receipt refers to the transaction, so the form can edit it"))))

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
      (is (empty? (entities/select (util/entity-type {} :receipt-ingestion)))
          "No receipt-ingestion is created"))))

(def ^:private ingestion-context
  (conj basic-context
        #:image{:user "john@doe.com"
                :original-filename "receipt.jpg"
                :content-type "image/jpeg"
                :content (io/file (io/resource "fixtures/attachment.jpg"))}
        #:transaction{:description "Kroger"
                      :entity "Personal"
                      :transaction-date (t/local-date 2015 1 2)
                      :quantity 10.5M
                      :debit-account "Groceries"
                      :credit-account "Checking"
                      :source :ingestion
                      :review-status :pending}
        #:receipt-ingestion{:entity "Personal"
                            :image "receipt.jpg"
                            :status :complete
                            :transaction [(t/local-date 2015 1 2) "Kroger"]}))

(defn- get-ingestion
  [email]
  (-> (request :get (path :api
                          :receipt-ingestions
                          (:id (find-receipt-ingestion "Personal")))
               :user (find-user email))
      ((build-app {:ingestion successful-reader}))
      parse-body))

(deftest a-user-can-get-a-receipt-ingestion-in-his-entity
  (with-context ingestion-context
    (let [{:as response :keys [parsed-body]} (get-ingestion "john@doe.com")]
      (is (http-success? response))
      (is (comparable? #:receipt-ingestion{:status :complete}
                       parsed-body)
          "The receipt-ingestion is returned"))))

(deftest a-user-cannot-get-a-receipt-ingestion-in-anothers-entity
  (with-context ingestion-context
    (is (http-not-found? (get-ingestion "jane@doe.com")))))

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
                     ((build-app {:ingestion successful-reader}))
                     parse-body)]
    [response
     (entities/find ingestion)
     (entities/find transaction)]))

(deftest a-user-can-reject-an-ingested-transaction-in-his-entity
  (with-context ingestion-context
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

(deftest a-reason-is-required-to-reject-an-ingested-transaction
  (with-context ingestion-context
    (let [[response ingestion transaction] (reject-ingestion "john@doe.com"
                                                             :reason "")]
      (is (http-bad-request? response))
      (is (comparable? #:receipt-ingestion{:status :complete}
                       ingestion)
          "The receipt-ingestion is not updated")
      (is transaction "The transaction is not deleted"))))

(deftest a-user-cannot-reject-an-ingested-transaction-in-anothers-entity
  (with-context ingestion-context
    (let [[response ingestion transaction] (reject-ingestion "jane@doe.com")]
      (is (http-not-found? response))
      (is (comparable? #:receipt-ingestion{:status :complete}
                       ingestion)
          "The receipt-ingestion is not updated")
      (is transaction "The transaction is not deleted"))))
