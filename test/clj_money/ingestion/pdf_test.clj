(ns clj-money.ingestion.pdf-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [dgknght.app-lib.test-assertions]
            [clj-money.ingestion.pdf :as pdf]
            [clj-money.ingestion.helpers :as hlp :refer [read-statement]])
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
  (io/file "fixtures/statements/checking.pdf"))

(deftest read-a-text-based-statement
  (let [{{[[{:keys [text]}
            _
            opts]] :read-statement
          :keys [close]} :calls
         result :result} (read-statement checking
                                         :provider ::pdf/pdf
                                         :model "m")]
    (is (= ::hlp/result
           result)
        "The inner reader result is returned")
    (is (comparable? {:model "m"}
                     opts)
        "The options used to create the reader are passed through to the function")
    (is (re-find #"Prairie State Bank" text)
        "The text is included")
    (is (re-find #"09/04\s+City Power & Light.+134\.22" text)
        "The columns of a table row are kept on the same line")
    (is (= 1 close)
        "The inner reader is closed")))

(deftest read-an-image-based-statement
  (let [{{[[{:keys [page-images text]}]] :read-statement} :calls}
        (read-statement (blank-pdf 2)
                        :dpi 72
                        :provider ::pdf/pdf)]
    (is (nil? text)
        "There is no text output")
    (is (= 2 (count page-images))
        "There is an image for each page")
    (is (= [612 792]
           (let [image (ImageIO/read (io/input-stream (first page-images)))]
             [(.getWidth image) (.getHeight image)]))
        "The page is rendered at the given resolution")))
