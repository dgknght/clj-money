(ns clj-money.ingestion)

(defprotocol Reader
  (read-receipt [_ source entity opts] "Read a receipt from photo, pdf, or email.")
  (close [_] "Release resources held by the instance."))

(defmulti reader ::provider)
