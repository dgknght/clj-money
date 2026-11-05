(ns clj-money.ingestion.scale
  "Scales images of receipts, and of the pages of statements, down before a
  model reads them"
  (:require [clj-money.io :refer [read-bytes]]
            [clj-money.images.transform :as transform]
            [clj-money.ingestion :as ing]))

(defn- longest-side
  [{:keys [width height]}]
  (max width height))

(defn shrink
  "Scales the image down so that its longest side is no longer than
  max-dimension, returning PNG bytes. The model's image token count grows
  with the pixel count, so this keeps the prompt small. The bytes are
  returned unchanged if the image is already small enough, or can't be
  decoded as an image (e.g., a PDF)."
  [image max-dimension]
  (or (transform/rescale image
                         (fn [dimensions]
                           (let [factor (/ max-dimension (longest-side dimensions))]
                             (when (< factor 1) factor))))
      image))

(defn enlarge
  "Scales the image up so that its longest side is at least min-dimension,
  returning PNG bytes. OCR reads the small text of a receipt photographed
  from a distance much better once it's enlarged. The bytes are returned
  unchanged if the image is already large enough, or can't be decoded as an
  image."
  [image min-dimension]
  (or (transform/rescale image
                         (fn [dimensions]
                           (let [factor (/ min-dimension (longest-side dimensions))]
                             (when (> factor 1) factor))))
      image))

(defn- shrink-source
  [source {:keys [max-image-dimension]
           :or {max-image-dimension 1280}}]
  (shrink (read-bytes source) max-image-dimension))

(defn- receipt-reader
  "Decorates a receipt reader, so that it's given the image scaled down (see
  scale-image) in place of the image. A receipt's contents, already read
  (e.g., the text of an email), are passed on as they are."
  [reader opts]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity read-opts]
      (ing/read-receipt reader
                        (if (map? source)
                          source
                          (shrink-source source opts))
                        entity
                        read-opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defn- statement-reader
  "Decorates a statement reader, so that the images of the pages of the
  statement it's given (see clj-money.ingestion.pdf) are scaled down (see
  scale-image). A statement's text is passed on as it is."
  [reader opts]
  (reify
    ing/StatementReader
    (read-statement [_ {:keys [page-images] :as contents} entity read-opts]
      (ing/read-statement reader
                          (if page-images
                            (assoc contents
                                   :page-images
                                   (mapv #(shrink-source % opts) page-images))
                            contents)
                          entity
                          read-opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::scale
  [{inner :reader :as config}]
  {:pre [inner]}
  (receipt-reader (ing/receipt-reader inner)
                  config))

(defmethod ing/statement-reader ::scale
  [{inner :reader :as config}]
  {:pre [inner]}
  (statement-reader (ing/statement-reader inner)
                    config))
