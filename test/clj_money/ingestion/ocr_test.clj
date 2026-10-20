(ns clj-money.ingestion.ocr-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.ocr :as ocr]
            [clj-money.ingestion.pdf-test :refer [blank-pdf]]
            [clj-money.ingestion.helpers :as hlp :refer [read-receipt]])
  (:import [java.io ByteArrayInputStream IOException]
           javax.imageio.ImageIO))

(def ^:private receipt
  (io/file "fixtures/sample_receipt.jpg"))

(defn- tesseract
  "Stands in for the tesseract command, recording each call"
  [calls & {:keys [out exit err] :or {out "SPROUTS\nTOTAL 38.97\n" exit 0}}]
  (fn [& args]
    (swap! calls conj args)
    {:exit exit :out out :err err}))

(defn- read-text
  "Reads the receipt with an OCR reader configured with the given settings,
  returning the text the inner reader is given"
  [source & {:as config}]
  (get-in (read-receipt source (assoc config :provider ::ocr/ocr))
          [:calls :read-receipt 0 0 :text]))

(defn- dimensions
  [image]
  (let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    [(.getWidth decoded) (.getHeight decoded)]))

(deftest the-text-of-an-image-is-read-by-tesseract
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (is (= "SPROUTS\nTOTAL 38.97\n" (read-text receipt))))
    (let [[args] @calls]
      (is (= ["tesseract" "stdin" "stdout" "-l" "eng" "--psm" "4"
              "-c" "thresholding_method=2"
              "-c" "thresholding_kfactor=0.2"
              "-c" "thresholding_window_size=1.0"
              :in]
             (butlast args))
          "The image is read with the default settings")
      (is (= [3000 2436] (dimensions (last args)))
          "The image is enlarged to 3000 pixels on its longest side"))))

(deftest an-image-large-enough-is-given-to-tesseract-unchanged
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (read-text receipt :min-image-dimension 300))
    (is (= (seq (.readAllBytes (io/input-stream receipt)))
           (seq (last (first @calls)))))))

(deftest a-small-image-is-enlarged-to-the-configured-size
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (read-text receipt :min-image-dimension 734))
    (is (= [734 596] (dimensions (last (first @calls)))))))

(deftest the-tesseract-settings-can-be-configured
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (read-text receipt :command "/opt/bin/tesseract" :language "spa" :psm 6))
    (is (= ["/opt/bin/tesseract" "stdin" "stdout" "-l" "spa" "--psm" "6"]
           (take 7 (first @calls))))))

(deftest the-text-of-a-pdf-is-read-without-tesseract
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls)]
      (is (re-find #"Prairie State Bank"
                   (read-text (io/file "fixtures/statements/checking.pdf")))))
    (is (empty? @calls))))

(deftest the-pages-of-a-scanned-pdf-are-read-by-tesseract
  (let [calls (atom [])]
    (with-redefs [shell/sh (tesseract calls :out "page")]
      (is (= "page\npage" (read-text (blank-pdf 2)))))
    (is (= 2 (count @calls))
        "Each page is read")))

(deftest a-receipt-without-text-cant-be-read
  (with-redefs [shell/sh (tesseract (atom []) :out " \n")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"No text could be read"
                          (read-text receipt)))))

(deftest a-tesseract-failure-is-explained
  (with-redefs [shell/sh (tesseract (atom []) :exit 1 :out "" :err "Error opening data file\n")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Tesseract OCR failed: Error opening data file$"
                          (read-text receipt)))))

(deftest a-missing-tesseract-is-explained
  (with-redefs [shell/sh (fn [& _] (throw (IOException. "Cannot run program")))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Is Tesseract OCR installed\?"
                          (read-text receipt)))))

(deftest a-receipt-reader-can-be-given-the-text-of-the-receipt
  (let [{:keys [result]
         {[[contents _ opts]] :read-receipt
          :keys [close]} :calls}
        (with-redefs [shell/sh (tesseract (atom []))]
          (read-receipt receipt :provider ::ocr/ocr :model "m"))]
    (is (= ::hlp/result result)
        "The inner reader result is returned")
    (is (= {:text "SPROUTS\nTOTAL 38.97\n"} contents)
        "The inner reader is given the text")
    (is (= "m" (:model opts))
        "The options are passed to the inner reader")
    (is (= 1 close)
        "The inner reader is closed")))

(defn- read-with-fallback
  "Reads the receipt with an OCR reader whose inner reader returns the
  result, given the text tesseract reads, and that falls back to another
  reader. Returns the result and the calls to each reader."
  [text result]
  (let [inner (hlp/inspectable-reader :result result)
        fallback (hlp/inspectable-reader :result ::fallback-result)
        reader (ing/receipt-reader {::ing/provider ::ocr/ocr
                                    :reader inner
                                    :fallback fallback})
        result (with-redefs [shell/sh (tesseract (atom []) :out text)]
                 (ing/read-receipt reader receipt {} {:model "m"}))]
    (ing/close reader)
    {:result result
     :inner (hlp/calls inner)
     :fallback (hlp/calls fallback)}))

(deftest a-total-in-the-text-is-read-without-the-fallback
  (let [{:keys [result fallback]} (read-with-fallback "TOTAL $38. 97\n"
                                                      {:total 38.97M})]
    (is (= {:total 38.97M} result)
        "The inner reader result is returned")
    (is (empty? (:read-receipt fallback))
        "The fallback reader isn't used")
    (is (= 1 (:close fallback))
        "The fallback reader is closed")))

(deftest a-total-with-a-thousands-separator-is-found-in-the-text
  (let [{:keys [result]} (read-with-fallback "TOTAL 1,234.50\n"
                                             {:total 1234.5M})]
    (is (= {:total 1234.5M} result))))

(deftest a-total-not-in-the-text-is-read-by-the-fallback
  (let [{:keys [result inner]
         {[[image _ opts]] :read-receipt} :fallback}
        (read-with-fallback "Amount: $28.67\n= Total 34 OF\n"
                            {:total 6.70M})]
    (is (= ::fallback-result result)
        "The fallback reader result is returned")
    (is (= 1 (count (:read-receipt inner)))
        "The inner reader reads the text first")
    (is (= (seq (.readAllBytes (io/input-stream receipt)))
           (seq image))
        "The fallback reader is given the image")
    (is (= "m" (:model opts))
        "The options are passed to the fallback reader")))

(deftest a-receipt-without-text-is-read-by-the-fallback
  (let [{:keys [result inner]} (read-with-fallback " \n" {:total 1M})]
    (is (= ::fallback-result result)
        "The fallback reader result is returned")
    (is (empty? (:read-receipt inner))
        "The inner reader isn't given blank text")))
