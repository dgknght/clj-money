(ns clj-money.ingestion.mock
  (:require [clj-money.ingestion :as ing]))

(defn- mock-reader
  []
  (reify
    ing/ReceiptReader
    (read-receipt [_ _source _entity _opts])
    ing/StatementReader
    (read-statement [_ _source _entity _opts])
    ing/Closeable
    (close [_])))

(defmethod ing/receipt-reader ::mock
  [_config]
  (mock-reader))

(defmethod ing/statement-reader ::mock
  [_config]
  (mock-reader))
