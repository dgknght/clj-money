(ns clj-money.ingestion.reader
  "Defines the integrant components for the document readers.

  There is a component for each kind of document, ::ing/receipt-reader and
  ::ing/statement-reader, and both are given the same config, which
  describes one reader for every kind of document:

    {:clj-money.ingestion/provider :clj-money.ingestion.gemini/gemini
     :api-key \"...\"}

  A kind of document can be given a reader of its own under :receipts or
  :statements, so that, say, receipts are read by Gemini while statements,
  which hold more sensitive information, are read by a local model:

    {:receipts {:clj-money.ingestion/provider :clj-money.ingestion.gemini/gemini
                :api-key \"...\"}
     :statements {:clj-money.ingestion/provider :clj-money.ingestion.pdf/pdf
                  :reader {:clj-money.ingestion/provider :clj-money.ingestion.ollama/ollama
                           :model \"qwen3:8b\"}}}

  A reader under :receipts or :statements takes nothing from the rest of the
  config, which applies only to a kind of document without one.

  The readers are composed in the config: a decorator, like the
  :clj-money.ingestion.pdf/pdf reader above, wraps the reader configured
  under its :reader (see clj-money.ingestion)."
  (:require [integrant.core :as ig]
            [clj-money.ingestion :as ing]))

(def ^:private document-types [:receipts :statements])

(defn- document-config
  [config document-type]
  (get config document-type (apply dissoc config document-types)))

(defmethod ig/init-key ::ing/receipt-reader
  [_ config]
  (ing/receipt-reader (document-config config :receipts)))

(defmethod ig/halt-key! ::ing/receipt-reader
  [_ reader]
  (ing/close reader))

(defmethod ig/init-key ::ing/statement-reader
  [_ config]
  (ing/statement-reader (document-config config :statements)))

(defmethod ig/halt-key! ::ing/statement-reader
  [_ reader]
  (ing/close reader))
