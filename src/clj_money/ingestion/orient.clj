(ns clj-money.ingestion.orient
  "Turns images of receipts upright before they're read. A phone saves a
  photo as the camera's sensor captured it and records the way to turn it
  upright in the photo's EXIF orientation tag, which OCR and the models
  don't look at, so they're given the receipt sideways or upside down."
  (:require [clj-money.io :refer [read-bytes]]
            [clj-money.images.transform :as transform]
            [clj-money.ingestion :as ing]))

(defn- upright
  "Returns the image turned upright, as PNG bytes, as its EXIF orientation
  says. The bytes are returned unchanged if the image is already upright,
  has no orientation, or isn't a JPEG (e.g., a PDF)."
  [image]
  (or (transform/upright image (transform/orientation image))
      image))

(defn- reader
  "Decorates a receipt reader, so that it's given the image turned upright
  in place of the image. A receipt's contents, already read (e.g., the text
  of an email, see clj-money.ingestion.email), are passed on as they are."
  [reader]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (ing/read-receipt reader
                        (if (map? source)
                          source
                          (upright (read-bytes source)))
                        entity
                        opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::orient
  [{inner :reader}]
  {:pre [inner]}
  (reader (ing/receipt-reader inner)))
