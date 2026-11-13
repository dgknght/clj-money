(ns clj-money.ingestion.pdf
  "Converts PDF content into either text or PNG images"
  (:require [clj-money.content-type :as ct]
            [clj-money.io :refer [read-bytes]]
            [clj-money.pdf :as pdf]
            [clj-money.ingestion :as ing]))

; A scanned page can still carry a little text, like a header added by the
; scanner, so a page needs more than that to count as having text
(def ^:private min-chars-per-page 50)

(defn pdf?
  "Returns true if the content, given as bytes, is a PDF"
  [content]
  (= "application/pdf" (ct/sniff content)))

(defn text
  "Returns the text of the PDF, given as bytes, or nil if it has too little
  text to read (e.g., the PDF was scanned from paper)"
  [content]
  (let [{:keys [text page-count]} (pdf/read-text content)]
    (when (<= (* min-chars-per-page page-count)
              (count (re-seq #"\S" text)))
      text)))

(defn- contents
  "Returns the contents of the given PDF as text if possible, otherwise as
  PNG images."
  [input & {:keys [dpi]}]
  (let [input (read-bytes input)]
    (if-let [text (text input)]
      {:text text}
      {:page-images (pdf/page-images input :dpi dpi)})))

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

(defn- receipt-source
  "Returns the images of the pages of a PDF receipt, as {:page-images [png
  ...]}, or, for an image, the image's bytes. A receipt's contents, already
  read (e.g., the text of an email), are returned as they are."
  [source {:keys [dpi]}]
  (if (map? source)
    source
    (let [content (read-bytes source)]
      (if (pdf? content)
        {:page-images (pdf/page-images content :dpi dpi)}
        content))))

(defn- receipt-reader
  "Decorates a receipt reader that can't open a PDF, so that a PDF receipt
  is given as images of its pages, {:page-images [png ...]}. An image is
  passed on as it is. Configure this before a
  :clj-money.ingestion.scale/scale reader to keep the images small."
  [reader {:keys [dpi] :or {dpi 200}}]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (ing/read-receipt reader
                        (receipt-source source {:dpi dpi})
                        entity
                        opts))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::pdf
  [{inner :reader :as opts}]
  {:pre [inner]}
  (receipt-reader (ing/receipt-reader inner) opts))

(defmethod ing/statement-reader ::pdf
  [{inner :reader :as opts}]
  {:pre [inner]}
  (reader (ing/statement-reader inner) opts))
