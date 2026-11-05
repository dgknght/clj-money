(ns clj-money.pdf
  "Reads the text of PDFs, and renders their pages as images, with PDFBox"
  (:import [java.io ByteArrayOutputStream]
           javax.imageio.ImageIO
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
