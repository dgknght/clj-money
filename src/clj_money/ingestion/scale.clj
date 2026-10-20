(ns clj-money.ingestion.scale
  "Scales images of receipts, and of the pages of statements, down before a
  model reads them"
  (:require [clojure.java.io :as io]
            [clj-money.ingestion :as ing])
  (:import [java.awt Color RenderingHints]
           java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           javax.imageio.ImageIO))

(defn- read-bytes
  ^bytes [input]
  (with-open [in (io/input-stream input)]
    (.readAllBytes in)))

(defn- resize
  ^BufferedImage [^BufferedImage image width height]
  (let [width (int width)
        height (int height)
        resized (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        g (.createGraphics resized)]
    (try
      (.setRenderingHint g
                         RenderingHints/KEY_INTERPOLATION
                         RenderingHints/VALUE_INTERPOLATION_BICUBIC)
      ; fill first, so any transparency becomes white instead of black
      (.setColor g Color/WHITE)
      (.fillRect g 0 0 width height)
      (.drawImage g image 0 0 width height nil)
      resized
      (finally
        (.dispose g)))))

(defn- longest-side
  [^BufferedImage image]
  (max (.getWidth image) (.getHeight image)))

(defn- rescale
  "Returns the image resized by the factor, as PNG bytes"
  ^bytes [^BufferedImage image factor]
  (let [out (ByteArrayOutputStream.)]
    (ImageIO/write (resize image
                           (max 1 (Math/round (double (* factor (.getWidth image)))))
                           (max 1 (Math/round (double (* factor (.getHeight image))))))
                   "png"
                   out)
    (.toByteArray out)))

(defn shrink
  "Scales the image down so that its longest side is no longer than
  max-dimension, returning PNG bytes. The model's image token count grows
  with the pixel count, so this keeps the prompt small. The bytes are
  returned unchanged if the image is already small enough, or can't be
  decoded as an image (e.g., a PDF)."
  [^bytes image max-dimension]
  (if-let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    (let [factor (/ max-dimension (longest-side decoded))]
      (if (< factor 1)
        (rescale decoded factor)
        image))
    image))

(defn enlarge
  "Scales the image up so that its longest side is at least min-dimension,
  returning PNG bytes. OCR reads the small text of a receipt photographed
  from a distance much better once it's enlarged. The bytes are returned
  unchanged if the image is already large enough, or can't be decoded as an
  image."
  [^bytes image min-dimension]
  (if-let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    (let [factor (/ min-dimension (longest-side decoded))]
      (if (> factor 1)
        (rescale decoded factor)
        image))
    image))

(defn- shrink-source
  [source {:keys [max-image-dimension]
           :or {max-image-dimension 1280}}]
  (shrink (read-bytes source) max-image-dimension))

(defn- receipt-reader
  "Decorates a receipt reader, so that it's given the image scaled down (see
  scale-image) in place of the image"
  [reader opts]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity read-opts]
      (ing/read-receipt reader
                        (shrink-source source opts)
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
