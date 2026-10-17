(ns clj-money.ingestion
  "Reads documents with a model. A provider can read receipts, statements,
  or both, and each kind of document is read by a system component of its
  own (see clj-money.ingestion.reader).

  A reader can decorate another, the one it's configured with under :reader,
  so that readers are composed in the config, e.g.:

    {:clj-money.ingestion/provider :clj-money.otel.ingestion/observable
     :reader {:clj-money.ingestion/provider :clj-money.ingestion.pdf/pdf
              :reader {:clj-money.ingestion/provider :clj-money.ingestion.ollama/ollama
                       :model \"qwen3:8b\"}}}")

(defprotocol ReceiptReader
  (read-receipt [_ source entity opts] "Read a receipt from photo, pdf, or email."))

(defprotocol StatementReader
  (read-statement [_ source entity opts]
    "Read an account statement. The source is the PDF or, for a reader that
    can't open a PDF, the PDF's contents: {:text \"...\"} or, for a PDF
    with no text (e.g., one scanned from paper), {:page-images [png-bytes ...]}."))

(defprotocol Closeable
  (close [_] "Release resources held by the instance."))

(defmulti receipt-reader
  "Returns a ReceiptReader (and Closeable) for the provider in the config"
  ::provider)

(defmulti statement-reader
  "Returns a StatementReader (and Closeable) for the provider in the config"
  ::provider)

(defmethod receipt-reader :default
  [{::keys [provider]}]
  (throw (ex-info (format "The %s reader can't read receipts." provider)
                  {:provider provider})))

(defmethod statement-reader :default
  [{::keys [provider]}]
  (throw (ex-info (format "The %s reader can't read statements." provider)
                  {:provider provider})))
