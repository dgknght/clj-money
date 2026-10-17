(ns clj-money.ingestion.pdf
  "Reads the contents of PDF documents, for models that can't read a PDF
  themselves"
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

(defn contents
  "Returns the contents of the PDF, as a statement reader that can't open a
  PDF reads them (see clj-money.ingestion/StatementReader)"
  [input & {:keys [dpi]}]
  (let [input (read-bytes input)] ; read twice when the pdf has no text
    (if-let [text (text input)]
      {:text text}
      {:page-images (page-images input :dpi dpi)})))

(defn reader
  "Decorates a statement reader that can't open a PDF, so that it's given the
  PDF's contents (see contents) in place of the PDF"
  [reader & {:keys [dpi] :or {dpi 200}}]
  (reify
    ing/StatementReader
    (read-statement [_ source entity opts]
      (ing/read-statement reader (contents source :dpi dpi) entity opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/statement-reader ::pdf
  [{inner :reader :keys [dpi] :or {dpi 200}}]
  {:pre [inner]}
  (reader (ing/statement-reader inner) :dpi dpi))
