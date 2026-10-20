(ns clj-money.ingestion.base64
  "Prepares images of receipts, and of the pages of statements, for models
  that are sent images base64-encoded"
  (:require [clojure.java.io :as io]
            [clj-money.ingestion :as ing])
  (:import java.util.Base64))

(defn encode
  "Returns the bytes read from the input, base64-encoded"
  [input]
  (with-open [input (io/input-stream input)]
    (.encodeToString (Base64/getEncoder)
                     (.readAllBytes input))))

(defn- receipt-reader
  "Decorates a receipt reader that is sent images base64-encoded, so that
  it's given {:images [base64 ...]} in place of the image. The image is sent
  as it is, so configure this under a :clj-money.ingestion.scale/scale
  reader to send a smaller one."
  [reader]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (ing/read-receipt reader
                        {:images [(encode source)]}
                        entity
                        opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defn- statement-reader
  "Decorates a statement reader that is sent images base64-encoded, so that
  the images of the pages of the statement it's given (see
  clj-money.ingestion.pdf) are given as {:images [base64 ...]}. A
  statement's text is passed on as it is."
  [reader]
  (reify
    ing/StatementReader
    (read-statement [_ {:keys [page-images] :as contents} entity opts]
      (ing/read-statement reader
                          (if page-images
                            {:images (mapv encode page-images)}
                            contents)
                          entity
                          opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::base64
  [{inner :reader}]
  {:pre [inner]}
  (receipt-reader (ing/receipt-reader inner)))

(defmethod ing/statement-reader ::base64
  [{inner :reader}]
  {:pre [inner]}
  (statement-reader (ing/statement-reader inner)))
