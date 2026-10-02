(ns clj-money.ingestion)

(defprotocol Reader
  (read-receipt [_ source] "Read a receipt from photo, pdf, or email."))

(defmulti reader ::provider)
