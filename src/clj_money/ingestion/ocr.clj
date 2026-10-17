(ns clj-money.ingestion.ocr
  "Reads the text of receipts with Tesseract OCR, so that a model is given
  the text in place of the image, which is much faster to read"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.pdf :as pdf])
  (:import java.io.IOException))

(defn- read-bytes
  ^bytes [input]
  (with-open [in (io/input-stream input)]
    (.readAllBytes in)))

(defn- pdf?
  [^bytes content]
  (and (<= 4 (alength content))
       (= "%PDF" (String. content 0 4 "US-ASCII"))))

(defn image-text
  "Returns the text read from the image by the tesseract command"
  [^bytes image {:keys [command language psm]
                 :or {command "tesseract"
                      language "eng"
                      ; a single column of text of varying sizes, as a
                      ; receipt is usually laid out
                      psm 4}}]
  (let [{:keys [exit out err]}
        (try
          (sh command "stdin" "stdout" "-l" language "--psm" (str psm)
              :in image)
          (catch IOException e
            (throw (ex-info (format "Unable to run %s. Is Tesseract OCR installed?"
                                    command)
                            {:command command}
                            e))))]
    (when-not (zero? exit)
      (throw (ex-info (format "Tesseract OCR failed: %s" (str/trim err))
                      {:exit exit})))
    out))

(defn text
  "Returns the text of the receipt, read from the image, or, for a PDF, the
  text of the PDF or, if it has none (e.g., it was scanned from paper), the
  text read from images of its pages"
  [source opts]
  (let [content (read-bytes source)
        text (if (pdf? content)
               (or (pdf/text content)
                   (->> (pdf/page-images content :dpi 300)
                        (map #(image-text % opts))
                        (str/join "\n")))
               (image-text content opts))]
    (when (str/blank? text)
      (throw (ex-info "No text could be read from the receipt." {})))
    text))

(defn reader
  "Decorates a receipt reader that can read text, so that it's given
  {:text \"...\"}, the text of the receipt, in place of the image"
  [reader & {:as opts}]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity read-opts]
      (ing/read-receipt reader
                        {:text (text source opts)}
                        entity
                        read-opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::ocr
  [{inner :reader :as config}]
  {:pre [inner]}
  (reader (ing/receipt-reader inner)
          (select-keys config [:command :language :psm])))
