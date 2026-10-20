(ns clj-money.ingestion.pdf
  "Converts PDF content into either text or PNG images"
  (:require [clojure.java.io :as io]
            [clj-money.ingestion :as ing])
  (:import [java.io ByteArrayOutputStream]
           javax.imageio.ImageIO
           org.apache.pdfbox.Loader
           org.apache.pdfbox.pdmodel.PDDocument
           [org.apache.pdfbox.rendering ImageType PDFRenderer]
           org.apache.pdfbox.text.PDFTextStripper))

(defn- read-bytes
  ^bytes [input]
  (with-open [in (io/input-stream input)]
    (.readAllBytes in)))

(defn- load-document
  ^PDDocument [input]
  (Loader/loadPDF (read-bytes input)))

; A scanned page can still carry a little text, like a header added by the
; scanner, so a page needs more than that to count as having text
(def ^:private min-chars-per-page 50)

(defn text
  "Returns the text of the PDF, or nil if it has too little text to read
  (e.g., the PDF was scanned from paper)"
  [input]
  (with-open [doc (load-document input)]
    (let [stripper (doto (PDFTextStripper.)
                     ; keeps the columns of a table on the same line
                     (.setSortByPosition true))
          text (.getText stripper doc)]
      (when (<= (* min-chars-per-page (.getNumberOfPages doc))
                (count (re-seq #"\S" text)))
        text))))

(defn page-images
  "Renders each page of the PDF, returning a sequence of PNG bytes"
  [input & {:keys [dpi] :or {dpi 150}}]
  (with-open [doc (load-document input)]
    (let [renderer (PDFRenderer. doc)]
      (mapv (fn [page]
              (let [out (ByteArrayOutputStream.)]
                (ImageIO/write (.renderImageWithDPI renderer
                                                    (int page)
                                                    (float dpi)
                                                    ImageType/RGB)
                               "png"
                               out)
                (.toByteArray out)))
            (range (.getNumberOfPages doc))))))

(defn- contents
  "Returns the contents of the given PDF as text if possible, otherwise as
  PNG images."
  [input & {:keys [dpi]}]
  (let [input (read-bytes input)]
    (if-let [text (text input)]
      {:text text}
      {:page-images (page-images input :dpi dpi)})))

(defn- reader
  "Decorates the given reader such that PDF input is translated to text or images"
  [reader {:keys [dpi] :or {dpi 200}}]
  (reify
    ing/StatementReader
    (read-statement [_ source entity opts]
      (ing/read-statement reader
                          (contents source :dpi dpi)
                          entity
                          opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/statement-reader ::pdf
  [{inner :reader :as opts}]
  {:pre [inner]}
  (reader (ing/statement-reader inner) opts))
