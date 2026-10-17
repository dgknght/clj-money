(ns clj-money.ingestion.ocr-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.ocr :as ocr]
            [clj-money.ingestion.pdf-test :refer [blank-pdf]])
  (:import java.io.IOException))

(def ^:private receipt
  (io/file "fixtures/sample_receipt.jpg"))

(defn- tesseract
  "Stands in for the tesseract command, recording each call"
  [calls & {:keys [out exit err] :or {out "SPROUTS\nTOTAL 38.97\n" exit 0}}]
  (fn [& args]
    (swap! calls conj args)
    {:exit exit :out out :err err}))

(deftest the-text-of-an-image-is-read-by-tesseract
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (is (= "SPROUTS\nTOTAL 38.97\n" (ocr/text receipt {}))))
    (let [[args] @calls]
      (is (= ["tesseract" "stdin" "stdout" "-l" "eng" "--psm" "4" :in]
             (butlast args))
          "The image is read with the default settings")
      (is (= (seq (.readAllBytes (io/input-stream receipt)))
             (seq (last args)))
          "The image is given to tesseract"))))

(deftest the-tesseract-settings-can-be-configured
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (ocr/text receipt {:command "/opt/bin/tesseract" :language "spa" :psm 6}))
    (is (= ["/opt/bin/tesseract" "stdin" "stdout" "-l" "spa" "--psm" "6"]
           (take 7 (first @calls))))))

(deftest the-text-of-a-pdf-is-read-without-tesseract
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (is (re-find #"Prairie State Bank"
                   (ocr/text (io/file "fixtures/statements/checking.pdf") {}))))
    (is (empty? @calls))))

(deftest the-pages-of-a-scanned-pdf-are-read-by-tesseract
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls :out "page")]
      (is (= "page\npage" (ocr/text (blank-pdf 2) {}))))
    (is (= 2 (count @calls))
        "Each page is read")))

(deftest a-receipt-without-text-cant-be-read
  (with-redefs [shell/sh (tesseract (atom []) :out " \n")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"No text could be read"
                          (ocr/text receipt {})))))

(deftest a-tesseract-failure-is-explained
  (with-redefs [shell/sh (tesseract (atom []) :exit 1 :out "" :err "Error opening data file\n")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Tesseract OCR failed: Error opening data file$"
                          (ocr/text receipt {})))))

(deftest a-missing-tesseract-is-explained
  (with-redefs [shell/sh (fn [& _] (throw (IOException. "Cannot run program")))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Is Tesseract OCR installed\?"
                          (ocr/text receipt {})))))

(def ^:private read-source (atom nil))
(def ^:private closed? (atom false))

(defmethod ing/receipt-reader ::recording
  [_config]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source _entity opts]
      (reset! read-source [source opts])
      {:total 38.97M})
    ing/Closeable
    (close [_] (reset! closed? true))))

(deftest a-receipt-reader-can-be-given-the-text-of-the-receipt
  (reset! closed? false)
  (let [calls (atom [])
        reader (ing/receipt-reader {::ing/provider ::ocr/ocr
                                    :psm 6
                                    :reader {::ing/provider ::recording}})]
    (with-redefs [shell/sh (tesseract calls)]
      (is (= {:total 38.97M}
             (ing/read-receipt reader receipt {:id 1} {:model "m"}))
          "The result of the decorated reader is returned"))
    (is (= [{:text "SPROUTS\nTOTAL 38.97\n"} {:model "m"}]
           @read-source)
        "The decorated reader is given the text and the options")
    (is (= "6" (nth (first @calls) 6))
        "The configured settings are used")
    (ing/close reader)
    (is @closed? "The decorated reader is closed")))
