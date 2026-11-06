(ns clj-money.pdf
  "Reads the text of PDFs, and renders their pages as images, with PDFBox,
  and renders HTML as a PDF, with openhtmltopdf"
  (:require [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream]
           java.util.function.BiPredicate
           javax.imageio.ImageIO
           [com.openhtmltopdf.extend FSUriResolver]
           [com.openhtmltopdf.outputdevice.helper ExternalResourceControlPriority]
           com.openhtmltopdf.pdfboxout.PdfRendererBuilder
           com.openhtmltopdf.util.XRLog
           org.jsoup.Jsoup
           org.jsoup.helper.W3CDom
           org.apache.pdfbox.Loader
           [org.apache.pdfbox.rendering ImageType PDFRenderer]
           org.apache.pdfbox.text.PDFTextStripper))

(defn read-text
  "Returns the :text of the PDF, given as bytes, and its :page-count"
  [^bytes content]
  (with-open [doc (Loader/loadPDF content)]
    (let [stripper (doto (PDFTextStripper.)
                     ; keeps the columns of a table on the same line
                     (.setSortByPosition true))]
      {:text (.getText stripper doc)
       :page-count (.getNumberOfPages doc)})))

(defn page-images
  "Renders each page of the PDF, given as bytes, returning a vector of PNG
  bytes"
  [^bytes content & {:keys [dpi] :or {dpi 150}}]
  (with-open [doc (Loader/loadPDF content)]
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

(defn- data-uri?
  [uri]
  (and (string? uri)
       (str/starts-with? (str/lower-case uri) "data:")))

; Nothing the HTML names is fetched, so that rendering an email neither
; tells its sender it was opened (a tracking pixel) nor makes the server
; send the requests an uploaded email asks for. Only what the HTML holds
; itself, as data URIs, is shown.
(def ^:private uri-resolver
  (reify FSUriResolver
    (resolveURI [_ _base uri]
      (when (data-uri? uri) uri))))

(def ^:private access-control
  (reify BiPredicate
    (test [_ uri _type]
      (data-uri? uri))))

(defn render-html
  "Renders the HTML, which needn't be well-formed, as a PDF, returning its
  bytes. Only resources given in place as data URIs are shown; nothing is
  fetched."
  [^String html]
  ; openhtmltopdf logs to java.util.logging, not through logback, and logs
  ; each remote image it refuses (every tracking pixel). A failure to
  ; render is thrown all the same.
  (XRLog/setLoggingEnabled false)
  (let [out (ByteArrayOutputStream.)]
    (doto (PdfRendererBuilder.)
      (.useFastMode)
      (.useUriResolver uri-resolver)
      (.useExternalResourceAccessControl access-control
                                         ExternalResourceControlPriority/RUN_BEFORE_RESOLVING_URI)
      (.useExternalResourceAccessControl access-control
                                         ExternalResourceControlPriority/RUN_AFTER_RESOLVING_URI)
      (.withW3cDocument (W3CDom/convert (Jsoup/parse html)) nil)
      (.toStream out)
      (.run))
    (.toByteArray out)))
