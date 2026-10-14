(ns clj-money.entities.receipt-ingestions
  (:require [clojure.spec.alpha :as s]
            [dgknght.app-lib.core :refer [update-in-if
                                          present?]]
            [dgknght.app-lib.validation :as v]
            [clj-money.entities :as entities]))

(s/def :receipt-ingestion/entity ::entities/entity-ref)
(s/def :receipt-ingestion/image ::entities/entity-ref)
(s/def :receipt-ingestion/status #{:pending :processing :complete :failed :rejected})
(s/def :receipt-ingestion/receipt (s/nilable map?))
(s/def :receipt-ingestion/error (s/nilable string?))
(s/def :receipt-ingestion/transaction (s/nilable ::entities/entity-ref))
(s/def :receipt-ingestion/rejection-reason (s/nilable string?))

(defn- rejection-has-reason?
  [{:receipt-ingestion/keys [status rejection-reason]}]
  (or (not= :rejected status)
      (present? rejection-reason)))
(v/reg-spec rejection-has-reason? {:message "A reason is required to reject the transaction"
                                   :path [:receipt-ingestion/rejection-reason]})

(s/def ::entities/receipt-ingestion (s/and (s/keys :req [:receipt-ingestion/entity
                                                         :receipt-ingestion/image
                                                         :receipt-ingestion/status]
                                                   :opt [:receipt-ingestion/receipt
                                                         :receipt-ingestion/error
                                                         :receipt-ingestion/transaction
                                                         :receipt-ingestion/rejection-reason])
                                           rejection-has-reason?))

; The receipt read from the image is stored as edn, so it keeps its shape
; (namespaced keys, dates, and decimals) in either storage
(defmethod entities/before-save :receipt-ingestion
  [ingestion]
  (update-in-if ingestion [:receipt-ingestion/receipt] pr-str))

(defmethod entities/after-read :receipt-ingestion
  [ingestion _]
  (update-in-if ingestion [:receipt-ingestion/receipt] read-string))
