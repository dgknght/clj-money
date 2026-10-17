(ns clj-money.ingestion.image-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.image :as image])
  (:import java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           java.util.Base64
           javax.imageio.ImageIO))

(defn- image-bytes
  [width height]
  (let [out (ByteArrayOutputStream.)]
    (ImageIO/write (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
                   "jpg"
                   out)
    (.toByteArray out)))

(defn- dimensions
  [image]
  (let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    [(.getWidth decoded) (.getHeight decoded)]))

(deftest a-large-image-is-scaled-down-for-the-model
  (is (= [960 1280]
         (dimensions (image/scale-image (image-bytes 1536 2048) 1280)))
      "The longest side is scaled to the maximum, keeping the aspect ratio"))

(deftest a-small-image-is-sent-unchanged
  (let [image (image-bytes 800 600)]
    (is (identical? image (image/scale-image image 1280)))))

(deftest a-file-that-isnt-an-image-is-sent-unchanged
  (let [pdf (.getBytes "%PDF-1.7")]
    (is (identical? pdf (image/scale-image pdf 1280)))))

(def ^:private read-source (atom nil))
(def ^:private closed? (atom false))

(defmethod ing/receipt-reader ::recording
  [_config]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source _entity opts]
      (reset! read-source [source opts])
      {:total 10M})
    ing/Closeable
    (close [_] (reset! closed? true))))

(deftest a-receipt-reader-can-be-given-the-image-base64-encoded
  (reset! closed? false)
  (let [reader (ing/receipt-reader {::ing/provider ::image/image
                                    :max-image-dimension 100
                                    :reader {::ing/provider ::recording}})
        result (ing/read-receipt reader (image-bytes 400 200) {:id 1} {:model "m"})
        [{:keys [images]} opts] @read-source]
    (is (= {:total 10M} result)
        "The result of the decorated reader is returned")
    (is (= 1 (count images))
        "The decorated reader is given the image")
    (is (= [100 50]
           (dimensions (.decode (Base64/getDecoder) ^String (first images))))
        "The image is base64-encoded and scaled to the configured maximum")
    (is (= {:model "m"} opts)
        "The options are passed through")
    (ing/close reader)
    (is @closed? "The decorated reader is closed")))
