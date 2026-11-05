(ns clj-money.images.transform
  "Reads and transforms images, given and returned as bytes, with ImageIO
  and Java 2D"
  (:import [java.awt Color RenderingHints]
           [java.awt.geom AffineTransform]
           java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           javax.imageio.ImageIO))

(def ^:private orientation-tag 0x0112)

(defn- exif-orientation
  "Returns the orientation in the EXIF TIFF structure that starts at offset,
  or nil if it has none"
  [^bytes image offset length]
  (let [buf (ByteBuffer/wrap image offset length)
        tiff (.slice buf)
        _ (.order tiff (if (= 0x4949 (.getShort tiff 0)) ; "II"
                         ByteOrder/LITTLE_ENDIAN
                         ByteOrder/BIG_ENDIAN))
        ifd (.getInt tiff 4)
        entries (.getShort tiff (int ifd))]
    (some (fn [i]
            (let [entry (+ ifd 2 (* 12 i))]
              (when (= orientation-tag (bit-and 0xFFFF (.getShort tiff (int entry))))
                (.getShort tiff (int (+ entry 8))))))
          (range entries))))

(defn- exif?
  [^bytes image offset]
  (= "Exif\u0000\u0000" (String. image (int offset) 6 "US-ASCII")))

(defn orientation
  "Returns the EXIF orientation of the JPEG image, a number from 1 (upright)
  through 8, or nil if it isn't a JPEG or has none"
  [^bytes image]
  (when (and (< 4 (alength image))
             (= 0xFF (bit-and 0xFF (aget image 0)))
             (= 0xD8 (bit-and 0xFF (aget image 1))))
    (try
      (loop [offset 2]
        (when (< (+ offset 4) (alength image))
          (let [marker (bit-and 0xFF (aget image (inc offset)))
                length (bit-or (bit-shift-left (bit-and 0xFF (aget image (+ offset 2))) 8)
                               (bit-and 0xFF (aget image (+ offset 3))))
                data (+ offset 4)]
            (cond
              ; the start of the image data, after which there are no tags
              (= 0xDA marker) nil

              (and (= 0xE1 marker) (exif? image data))
              (exif-orientation image (+ data 6) (- length 8))

              :else (recur (+ offset 2 length))))))
      ; a malformed header is read as it is, as it would be without this
      (catch IndexOutOfBoundsException _ nil))))

(defn- decode
  ^BufferedImage [^bytes image]
  (ImageIO/read (ByteArrayInputStream. image)))

(defn- ->png
  ^bytes [^BufferedImage image]
  (let [out (ByteArrayOutputStream.)]
    (ImageIO/write image "png" out)
    (.toByteArray out)))

; The transform that turns an image with each orientation upright, as
; [m00 m10 m01 m11 m02 m12], given the width and height of the image as
; it's stored. Orientations 5 through 8 swap the width and height.
(def ^:private transforms
  {2 (fn [w _] [-1 0 0 1 w 0])    ; mirrored
   3 (fn [w h] [-1 0 0 -1 w h])   ; upside down
   4 (fn [_ h] [1 0 0 -1 0 h])    ; mirrored upside down
   5 (fn [_ _] [0 1 1 0 0 0])     ; mirrored and turned
   6 (fn [_ h] [0 1 -1 0 h 0])    ; turned counterclockwise, from a phone held upright
   7 (fn [w h] [0 -1 -1 0 h w])   ; mirrored and turned
   8 (fn [w _] [0 -1 1 0 0 w])})  ; turned clockwise

(defn- turn
  ^BufferedImage [^BufferedImage image orientation]
  (let [width (.getWidth image)
        height (.getHeight image)
        [m00 m10 m01 m11 m02 m12] ((transforms orientation) width height)
        upright (if (<= 5 orientation)
                  (BufferedImage. height width BufferedImage/TYPE_INT_RGB)
                  (BufferedImage. width height BufferedImage/TYPE_INT_RGB))
        g (.createGraphics upright)]
    (try
      (.drawImage g image (AffineTransform. (double m00) (double m10)
                                            (double m01) (double m11)
                                            (double m02) (double m12))
                  nil)
      upright
      (finally
        (.dispose g)))))

(defn upright
  "Returns the image turned upright from the EXIF orientation given, as PNG
  bytes, or nil if the orientation doesn't need turning (e.g., 1, or nil)
  or the image can't be decoded"
  [^bytes image orientation]
  (when-let [decoded (when (transforms orientation)
                       (decode image))]
    (->png (turn decoded orientation))))

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

(defn rescale
  "Calls f with the image's dimensions, {:width w :height h}, and returns
  the image resized by the factor f returns, as PNG bytes. Returns nil if f
  returns nil, or the image can't be decoded (e.g., it's a PDF)."
  [^bytes image f]
  (when-let [decoded (decode image)]
    (let [width (.getWidth decoded)
          height (.getHeight decoded)]
      (when-let [factor (f {:width width :height height})]
        (->png (resize decoded
                       (max 1 (Math/round (double (* factor width))))
                       (max 1 (Math/round (double (* factor height))))))))))
