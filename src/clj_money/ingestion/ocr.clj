(ns clj-money.ingestion.ocr
  "Reads the text of receipts with Tesseract OCR, so that a model is given
  the text in place of the image, which is much faster to read. OCR can't
  read everything a model reading the image can, like a handwritten tip, so
  nil is returned for a receipt whose text can't be read, or that the model
  can't find the total in, so that another reader can read the image instead
  (see clj-money.ingestion.logic)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.pdf :as pdf]
            [clj-money.ingestion.scale :as scale])
  (:import java.io.IOException))

(defn- read-bytes
  ^bytes [input]
  (with-open [in (io/input-stream input)]
    (.readAllBytes in)))

(defn- pdf?
  [^bytes content]
  (and (<= 4 (alength content))
       (= "%PDF" (String. content 0 4 "US-ASCII"))))

(defn- image-text
  "Returns the text read from the image by the tesseract command. A small
  image is enlarged first, as Tesseract misreads text much less than about
  30 pixels high."
  [^bytes image {:keys [command language psm min-image-dimension]
                 :or {command "tesseract"
                      language "eng"
                      ; a single column of text of varying sizes, as a
                      ; receipt is usually laid out
                      psm 4
                      min-image-dimension 3000}}]
  (let [{:keys [exit out err]}
        (try
          (sh command "stdin" "stdout" "-l" language "--psm" (str psm)
              ; Sauvola thresholding separates the text from the paper in
              ; each part of the image, so the shadows and uneven lighting of
              ; a photographed receipt don't wash out the text, as they do
              ; with the default Otsu thresholding of the whole image. The
              ; window is larger than the default, which washes out a
              ; receipt's faint print.
              "-c" "thresholding_method=2"
              "-c" "thresholding_kfactor=0.2"
              "-c" "thresholding_window_size=1.0"
              :in (scale/enlarge image min-image-dimension))
          (catch IOException e
            (throw (ex-info (format "Unable to run %s. Is Tesseract OCR installed?"
                                    command)
                            {:command command}
                            e))))]
    (when-not (zero? exit)
      (throw (ex-info (format "Tesseract OCR failed: %s" (str/trim err))
                      {:exit exit})))
    out))

(defn- text
  "Returns the text of the receipt, read from the image, or, for a PDF, the
  text of the PDF or, if it has none (e.g., it was scanned from paper), the
  text read from images of its pages"
  [^bytes content opts]
  (let [text (if (pdf? content)
               (or (pdf/text content)
                   (->> (pdf/page-images content :dpi 300)
                        (map #(image-text % opts))
                        (str/join "\n")))
               (image-text content opts))]
    (log/debugf "ocr result: %s" (pr-str text))
    text))

(defn- amounts
  "Returns the amounts in the text, allowing for the stray spaces and
  commas OCR reads into them, e.g., \"$58. 30\" or \"$13,99\""
  [text]
  (map (fn [[_ whole cents]]
         (bigdec (str (str/replace whole "," "") "." cents)))
       (re-seq #"(\d{1,3}(?:,\d{3})+|\d+)[.,] ?(\d{2})(?!\d)" text)))

(defn- total-in-text?
  "Returns true if the total the model read is in the text. A total that
  isn't was most likely made up from text OCR misread, like a handwritten
  total."
  [{:keys [total]} text]
  (boolean (and total
                (some #(== total %) (amounts text)))))

(defn- read-text
  "Reads the receipt with the reader, given its text. Returns nil if there's
  no text or the total read isn't in it, so that a reader that can read what
  OCR can't (see clj-money.ingestion.logic) can read it instead."
  [reader content entity opts read-opts]
  (let [text (text content opts)]
    (if (str/blank? text)
      (log/info "No text could be read from the receipt.")
      (let [result (ing/read-receipt reader {:text text} entity read-opts)]
        (if (total-in-text? result text)
          result
          (log/infof "The total read, %s, isn't in the text of the receipt."
                     (:total result)))))))

(defn- reader
  "Decorates a receipt reader that can read text, so that it's given
  {:text \"...\"}, the text of the receipt, in place of the image. Returns
  nil when no text can be read from the image or the total read isn't in
  the text."
  [reader opts]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity read-opts]
      (read-text reader
                 (read-bytes source)
                 entity
                 opts
                 read-opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::ocr
  [{inner :reader :as config}]
  {:pre [inner]}
  (reader (ing/receipt-reader inner)
          (select-keys config [:command :language :psm :min-image-dimension])))
