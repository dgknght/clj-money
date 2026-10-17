(ns clj-money.ingestion.image
  "Prepares images of receipts for models that are sent images base64-encoded"
  (:require [clojure.java.io :as io]
            [clj-money.ingestion :as ing])
  (:import java.util.Base64
           [java.awt Color RenderingHints]
           java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           javax.imageio.ImageIO))

(defn ->base64
  [input]
  (let [input (io/input-stream input)]
    (.encodeToString (Base64/getEncoder)
                     (.readAllBytes input))))

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

(defn scale-image
  "Scales the image down so that its longest side is no longer than
  max-dimension, returning PNG bytes. The model's image token count grows
  with the pixel count, so this keeps the prompt small. The bytes are
  returned unchanged if the image is already small enough, or can't be
  decoded as an image (e.g., a PDF)."
  ^bytes [^bytes image max-dimension]
  (if-let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    (let [width (.getWidth decoded)
          height (.getHeight decoded)
          scale (/ max-dimension (max width height))]
      (if (< scale 1)
        (let [out (ByteArrayOutputStream.)]
          (ImageIO/write (resize decoded
                                 (max 1 (Math/round (double (* scale width))))
                                 (max 1 (Math/round (double (* scale height)))))
                         "png"
                         out)
          (.toByteArray out))
        image))
    image))

(defn image-for-model
  "Returns the base64-encoded image, scaled down for the model"
  [source {:keys [max-image-dimension]
           :or {max-image-dimension 1280}}]
  (->base64 (scale-image (read-bytes source) max-image-dimension)))

(defn reader
  "Decorates a receipt reader that is sent images base64-encoded, so that
  it's given {:images [base64 ...]} in place of the image"
  [reader & {:as opts}]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity read-opts]
      (ing/read-receipt reader
                        {:images [(image-for-model source opts)]}
                        entity
                        read-opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::image
  [{inner :reader :keys [max-image-dimension]}]
  {:pre [inner]}
  (reader (ing/receipt-reader inner)
          :max-image-dimension (or max-image-dimension 1280)))
