(ns clj-money.ingestion.orient-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.ingestion.orient :as orient]
            [clj-money.ingestion.helpers :as hlp :refer [read-receipt]])
  (:import [java.awt Color]
           java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           javax.imageio.ImageIO))

(defn- jpeg
  "Returns a 40x20 JPEG, red on the left half and blue on the right"
  []
  (let [image (BufferedImage. 40 20 BufferedImage/TYPE_INT_RGB)
        g (.createGraphics image)
        out (ByteArrayOutputStream.)]
    (.setColor g Color/RED)
    (.fillRect g 0 0 20 20)
    (.setColor g Color/BLUE)
    (.fillRect g 20 0 20 20)
    (.dispose g)
    (ImageIO/write image "jpg" out)
    (.toByteArray out)))

(defn- exif-segment
  "Returns an APP1 segment holding EXIF with the orientation"
  [orientation byte-order]
  (let [tiff (doto (ByteBuffer/allocate 26)
               (.order byte-order)
               (.put (.getBytes (if (= ByteOrder/LITTLE_ENDIAN byte-order) "II" "MM")))
               (.putShort 42)
               (.putInt 8)       ; the offset of the first directory
               (.putShort 1)     ; with one entry
               (.putShort 0x0112)
               (.putShort 3)     ; a short
               (.putInt 1)
               (.putShort orientation)
               (.putShort 0)
               (.putInt 0))      ; with no next directory
        length (+ 2 6 26)]
    (doto (ByteBuffer/allocate (+ 2 length))
      (.put (unchecked-byte 0xFF))
      (.put (unchecked-byte 0xE1))
      (.putShort length)
      (.put (.getBytes "Exif\u0000\u0000" "US-ASCII"))
      (.put (.array tiff)))))

(defn- oriented-jpeg
  "Returns the jpeg with an EXIF segment, after its JFIF segment, giving
  the orientation"
  [orientation & {:keys [byte-order] :or {byte-order ByteOrder/BIG_ENDIAN}}]
  (let [image (jpeg)
        ; the SOI marker and the JFIF APP0 segment
        head (+ 4 (bit-or (bit-shift-left (bit-and 0xFF (aget image 4)) 8)
                          (bit-and 0xFF (aget image 5))))
        segment (.array (exif-segment orientation byte-order))]
    (byte-array (concat (take head image)
                        segment
                        (drop head image)))))

(defn- upright
  "Returns the image the inner reader is given when the orient reader reads
  the receipt"
  [receipt]
  (get-in (read-receipt receipt :provider ::orient/orient)
          [:calls :read-receipt 0 0]))

(defn- colors
  "Returns the color of the top-left and bottom-right corners of the image,
  along with its dimensions"
  [image]
  (let [decoded (ImageIO/read (ByteArrayInputStream. image))
        width (.getWidth decoded)
        height (.getHeight decoded)
        color (fn [x y]
                (let [c (Color. (.getRGB decoded (int x) (int y)))]
                  (if (> (.getRed c) (.getBlue c)) :red :blue)))]
    {:size [width height]
     :top-left (color 2 2)
     :bottom-right (color (- width 3) (- height 3))}))

(deftest an-image-turned-counterclockwise-is-turned-upright
  (is (= {:size [20 40] :top-left :red :bottom-right :blue}
         (colors (upright (oriented-jpeg 6))))
      "The image is turned clockwise, so the left side is on top"))

(deftest an-image-turned-clockwise-is-turned-upright
  (is (= {:size [20 40] :top-left :blue :bottom-right :red}
         (colors (upright (oriented-jpeg 8))))
      "The image is turned counterclockwise, so the left side is on the bottom"))

(deftest an-upside-down-image-is-turned-upright
  (is (= {:size [40 20] :top-left :blue :bottom-right :red}
         (colors (upright (oriented-jpeg 3))))))

(deftest a-mirrored-image-is-flipped
  (is (= {:size [40 20] :top-left :blue :bottom-right :red}
         (colors (upright (oriented-jpeg 2))))))

(deftest the-orientation-is-read-in-either-byte-order
  (is (= {:size [20 40] :top-left :red :bottom-right :blue}
         (colors (upright (oriented-jpeg 6 :byte-order ByteOrder/LITTLE_ENDIAN))))))

(deftest an-upright-image-is-passed-on-unchanged
  (let [image (oriented-jpeg 1)]
    (is (= (seq image) (seq (upright image))))))

(deftest an-image-without-an-orientation-is-passed-on-unchanged
  (let [image (jpeg)]
    (is (= (seq image) (seq (upright image))))))

(deftest a-pdf-is-passed-on-unchanged
  (let [pdf (.getBytes "%PDF-1.7")]
    (is (= (seq pdf) (seq (upright pdf))))))

(deftest a-receipt-reader-can-be-given-the-image-upright
  (let [{:keys [result]
         {[[_ _ opts]] :read-receipt
          :keys [close]} :calls}
        (read-receipt (ByteArrayInputStream. (oriented-jpeg 6))
                      :provider ::orient/orient
                      :model "m")]
    (is (= ::hlp/result result)
        "The inner reader result is returned")
    (is (= "m" (:model opts))
        "The options are passed to the inner reader")
    (is (= 1 close)
        "The inner reader is closed")))
