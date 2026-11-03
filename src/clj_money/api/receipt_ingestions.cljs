(ns clj-money.api.receipt-ingestions
  (:refer-clojure :exclude [get])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  "Uploads a receipt image, given as the {:blob :url} map produced by
  dgknght.app-lib.forms/image-input, or an email, given as {:blob file
  :filename \"...\"}, and starts reading it. The response is the
  receipt-ingestion that tracks the progress of the read."
  [{:keys [blob filename]
    :or {filename "receipt.jpg"}}
   & {:as opts}]
  (api/post (api/path :entities
                      @current-entity
                      :receipt-ingestions)
            {:image [blob filename]}
            (-> opts
                ; the encoding would otherwise set the Accept header to
                ; application/multipart
                (assoc :encoding :multipart
                       :accept "application/edn")
                (add-error-handler "Unable to upload the receipt: %s"))))

(defn get
  "Retrieves the receipt-ingestion, to check on the progress of the read."
  [{:keys [id]} & {:as opts}]
  (api/get (api/path :receipt-ingestions id)
           {}
           (add-error-handler
             opts
             "Unable to retrieve the receipt progress: %s")))

(defn select-for-transactions
  "Retrieves the receipt-ingestions that created the given transactions"
  [transactions & {:as opts}]
  (api/get (api/path :entities
                     @current-entity
                     :receipt-ingestions)
           {:transaction-id (mapv :id transactions)}
           (add-error-handler
             opts
             "Unable to retrieve the receipts: %s")))

(defn reject
  "Rejects the transaction created from the receipt, which deletes it. The
  reason is required."
  [ingestion reason & {:as opts}]
  (api/patch (api/path :receipt-ingestions (:id ingestion))
             #:receipt-ingestion{:status :rejected
                                 :rejection-reason reason}
             (add-error-handler
               opts
               "Unable to reject the transaction: %s")))
