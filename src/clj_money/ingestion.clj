(ns clj-money.ingestion
  (:require [integrant.core :as ig]))

(defprotocol Reader
  (read-receipt [_ source entity] "Read a receipt from photo, pdf, or email.")
  (close [_] "Release resources held by the instance."))

(defmulti reader ::provider)

(defmethod ig/init-key ::reader
  [_ config]
  (reader config))

(defmethod ig/halt-key! ::reader
  [_ reader]
  (close reader))
