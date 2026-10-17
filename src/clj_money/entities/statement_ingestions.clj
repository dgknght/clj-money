(ns clj-money.entities.statement-ingestions
  (:require [clojure.spec.alpha :as s]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.entities :as entities]))

(s/def :statement-ingestion/entity ::entities/entity-ref)
(s/def :statement-ingestion/image ::entities/entity-ref)
(s/def :statement-ingestion/account (s/nilable ::entities/entity-ref))
(s/def :statement-ingestion/status #{:pending :processing :complete :failed})
(s/def :statement-ingestion/statement (s/nilable map?))
(s/def :statement-ingestion/warnings (s/nilable (s/coll-of string?)))
(s/def :statement-ingestion/error (s/nilable string?))

(s/def ::entities/statement-ingestion (s/keys :req [:statement-ingestion/entity
                                                    :statement-ingestion/image
                                                    :statement-ingestion/status]
                                              :opt [:statement-ingestion/account
                                                    :statement-ingestion/statement
                                                    :statement-ingestion/warnings
                                                    :statement-ingestion/error]))

; The statement read from the document is stored as edn, so it keeps its
; shape (dates and decimals) in either storage
(defmethod entities/before-save :statement-ingestion
  [ingestion]
  (-> ingestion
      (update-in-if [:statement-ingestion/statement] pr-str)
      (update-in-if [:statement-ingestion/warnings] pr-str)))

(defmethod entities/after-read :statement-ingestion
  [ingestion _]
  (-> ingestion
      (update-in-if [:statement-ingestion/statement] read-string)
      (update-in-if [:statement-ingestion/warnings] read-string)))
