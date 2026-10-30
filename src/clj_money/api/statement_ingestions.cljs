(ns clj-money.api.statement-ingestions
  (:refer-clojure :exclude [get update])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  "Uploads a statement, given as a js/File, and starts reading it. When the
  account is given, the statement is reconciled against it; otherwise the
  account is identified from the statement. The response is the
  statement-ingestion that tracks the progress of the read."
  [file & {:keys [account] :as opts}]
  (api/post (api/path :entities
                      @current-entity
                      :statement-ingestions)
            (cond-> {:statement [file (.-name file)]}
              account (assoc :account-id (str (:id account))))
            (-> opts
                (dissoc :account)
                ; the encoding would otherwise set the Accept header to
                ; application/multipart
                (assoc :encoding :multipart
                       :accept "application/edn")
                (add-error-handler "Unable to upload the statement: %s"))))

(defn get
  "Retrieves the statement-ingestion, to check on the progress of the read."
  [{:keys [id]} & {:as opts}]
  (api/get (api/path :statement-ingestions id)
           {}
           (add-error-handler
             opts
             "Unable to retrieve the statement progress: %s")))

(defn update
  "Corrects the statement read for a statement-ingestion that couldn't be
  reconciled, and reconciles it again. The corrections are given as
  #:statement-ingestion{:account ... :statement {...}}. The response is the
  statement-ingestion, which tracks the progress of the reconciliation."
  [{:keys [id]} corrections & {:as opts}]
  (api/patch (api/path :statement-ingestions id)
             corrections
             (add-error-handler
               opts
               "Unable to correct the statement: %s")))
