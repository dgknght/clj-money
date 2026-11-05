(ns clj-money.ingestion.pdf
  "Converts PDF content into either text or PNG images"
  (:require [clj-money.io :refer [read-bytes]]
            [clj-money.pdf :as pdf]
            [clj-money.ingestion :as ing]))

; A scanned page can still carry a little text, like a header added by the
; scanner, so a page needs more than that to count as having text
(def ^:private min-chars-per-page 50)

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

(defmethod ing/statement-reader ::pdf
  [{inner :reader :as opts}]
  {:pre [inner]}
  (reader (ing/statement-reader inner) opts))
