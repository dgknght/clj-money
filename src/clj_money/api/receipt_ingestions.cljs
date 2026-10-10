(ns clj-money.api.receipt-ingestions
  (:refer-clojure :exclude [get])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  "Uploads a receipt image, given as the {:blob :url} map produced by
  dgknght.app-lib.forms/image-input, and starts reading it. The response
  is the receipt-ingestion that tracks the progress of the read."
  [{:keys [blob]} & {:as opts}]
  (api/post (api/path :entities
                      @current-entity
                      :receipt-ingestions)
            {:image [blob "receipt.jpg"]}
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
