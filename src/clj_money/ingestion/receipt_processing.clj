(ns clj-money.ingestion.receipt-processing
  "Reads a receipt that was uploaded or emailed, tracked by a
  receipt-ingestion, and creates the transaction for the user to review"
  (:require [clojure.tools.logging :as log]
            [clj-money.util :as util]
            [clj-money.images :as images]
            [clj-money.entities :as entities]
            [clj-money.entities.images :as img]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.ingestion.email :as email]
            [clj-money.mime :as mime]))

(defn- extracted-attachments
  "Returns an attachment to the transaction for each image or PDF attached
  to the email uploaded as the receipt, so that each can be viewed without
  opening the email. Anything other than an email has none."
  [content user trx]
  (when (mime/message? content)
    (mapv (fn [{:keys [mime-type filename] :as part}]
            #:attachment{:transaction trx
                         :image (util/->entity-ref
                                  (img/find-or-create
                                    #:image{:user user
                                            :original-filename (or filename "attachment")
                                            :content-type mime-type
                                            :content (:content part)}))
                         :caption (or filename "Email attachment")})
          (email/attachments content))))

(defn- create-transaction
  [{:receipt-ingestion/keys [image] :as ingestion}
   receipt
   entity
   account-opts
   {:keys [content user]}]
  (let [trx (assoc (rcpts/make-trx receipt
                                   entity
                                   :account-options account-opts)
                   :id (util/temp-id))
        att #:attachment{:transaction trx
                         :image image
                         :caption "Receipt"}
        saved (last (entities/put-many
                      (concat [trx att]
                              (extracted-attachments content user trx)
                              [(assoc ingestion
                                      :receipt-ingestion/transaction (util/->entity-ref trx)
                                      :receipt-ingestion/receipt receipt)])))]
    (-> saved
        (assoc :receipt-ingestion/status :complete)
        entities/put)))

(defn process
  "Reads the receipt of the ingestion (its image, or an email) with the
  reader and, if it is one, creates the transaction for the user to review.
  Returns the ingestion, complete or failed."
  [ingestion reader]
  (let [{:receipt-ingestion/keys [entity image] :as ingestion}
        (entities/put (assoc ingestion :receipt-ingestion/status :processing))
        entity (entities/find entity)]
    (try
      (let [account-opts (rcpts/account-options entity)
            {:image/keys [uuid user]} (entities/find image)
            content (images/get uuid)
            receipt (ing/read-receipt reader
                                      content
                                      entity
                                      {:account-options account-opts})]
        ; an uploaded email may turn out not to be a receipt
        (if (false? (:is-receipt receipt))
          (entities/put (assoc ingestion
                               :receipt-ingestion/status :failed
                               :receipt-ingestion/failure-reason :not-a-receipt
                               :receipt-ingestion/error "That isn't a receipt."
                               :receipt-ingestion/receipt receipt))
          (create-transaction ingestion
                              receipt
                              entity
                              account-opts
                              {:content content
                               :user user})))
      (catch Exception e
        (log/errorf e "[receipt-ingestion] unable to read receipt %s" (:id ingestion))
        (entities/put (assoc ingestion
                             :receipt-ingestion/status :failed
                             :receipt-ingestion/error (ex-message e)))))))
