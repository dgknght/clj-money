(ns clj-money.ingestion.reader
  "Defines the integrant component for the receipt reader, kept apart from
  clj-money.ingestion so that any reader implementation can be decorated
  without a circular reference."
  (:require [integrant.core :as ig]
            [clj-money.ingestion :as ing]
            [clj-money.otel.ingestion :as otel-ing]))

(defmethod ig/init-key ::ing/reader
  [_ config]
  (otel-ing/observable-reader (ing/reader config) config))

(defmethod ig/halt-key! ::ing/reader
  [_ reader]
  (ing/close reader))
