(ns clj-money.ingestion.pdf-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.pdf :as pdf])
  (:import java.io.ByteArrayOutputStream
           javax.imageio.ImageIO
           [org.apache.pdfbox.pdmodel PDDocument PDPage]))

(defn blank-pdf
  "Returns the bytes of a PDF with the given number of blank pages, like one
  scanned from paper, which has no text"
  [pages]
  (with-open [doc (PDDocument.)]
    (dotimes [_ pages]
      (.addPage doc (PDPage.)))
    (let [out (ByteArrayOutputStream.)]
      (.save doc out)
      (.toByteArray out))))

(def ^:private checking
  (io/resource "fixtures/statements/checking.pdf"))

(deftest the-text-of-a-pdf-is-read
  (let [text (pdf/text checking)]
    (is (re-find #"Prairie State Bank" text)
        "The text is included")
    (is (re-find #"09/04\s+City Power & Light.+134\.22" text)
        "The columns of a table row are kept on the same line")))

(deftest a-pdf-without-text-has-no-text
  (is (nil? (pdf/text (blank-pdf 2)))))

(deftest each-page-of-a-pdf-is-rendered-as-an-image
  (let [images (pdf/page-images (blank-pdf 2) :dpi 72)]
    (is (= 2 (count images))
        "There is an image for each page")
    (is (= [612 792]
           (let [image (ImageIO/read (io/input-stream (first images)))]
             [(.getWidth image) (.getHeight image)]))
        "The page is rendered at the given resolution")))

(deftest the-contents-of-a-pdf-with-text-are-its-text
  (is (re-find #"Prairie State Bank" (:text (pdf/contents checking)))))

(deftest the-contents-of-a-pdf-without-text-are-images-of-its-pages
  (let [{:keys [text page-images]} (pdf/contents (blank-pdf 2) :dpi 72)]
    (is (nil? text))
    (is (= 2 (count page-images)))))

(deftest a-statement-reader-can-be-given-the-contents-of-the-pdf
  (let [read (atom nil)
        closed? (atom false)
        reader (pdf/reader
                 (reify
                   ing/StatementReader
                   (read-statement [_ source _entity opts]
                     (reset! read [source opts])
                     {:closing-balance 10M})
                   ing/Closeable
                   (close [_] (reset! closed? true))))]
    (is (= {:closing-balance 10M}
           (ing/read-statement reader checking {:id 1} {:model "m"}))
        "The result of the decorated reader is returned")
    (is (re-find #"Prairie State Bank" (get-in @read [0 :text]))
        "The decorated reader is given the contents of the pdf")
    (is (= {:model "m"} (second @read))
        "The options are passed through")
    (ing/close reader)
    (is @closed? "The decorated reader is closed")))

(def ^:private read-source (atom nil))

(defmethod ing/statement-reader ::recording
  [_config]
  (reify
    ing/StatementReader
    (read-statement [_ source _entity _opts]
      (reset! read-source source)
      {})
    ing/Closeable
    (close [_])))

(deftest a-pdf-reader-can-be-configured
  (ing/read-statement (ing/statement-reader {::ing/provider ::pdf/pdf
                                             :dpi 72
                                             :reader {::ing/provider ::recording}})
                      (blank-pdf 1)
                      {:id 1}
                      {})
  (is (= [612 792]
         (let [image (ImageIO/read (io/input-stream (first (:page-images @read-source))))]
           [(.getWidth image) (.getHeight image)]))
      "The configured reader is given the contents at the configured resolution"))
