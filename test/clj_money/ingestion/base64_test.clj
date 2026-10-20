(ns clj-money.ingestion.base64-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.base64 :as base64]
            [clj-money.ingestion.scale :as scale]
            [clj-money.ingestion.pdf :as pdf]
            [clj-money.ingestion.pdf-test :refer [blank-pdf]]
            [clj-money.ingestion.helpers :as hlp :refer [inspectable-reader
                                                          read-receipt
                                                          read-statement]])
  (:import java.io.ByteArrayInputStream
           java.util.Base64
           javax.imageio.ImageIO))

(def ^:private receipt
  (io/file "fixtures/sample_receipt.jpg"))

(defn- decode
  [^String s]
  (.decode (Base64/getDecoder) s))

(deftest a-receipt-reader-can-be-given-the-image-base64-encoded
  (let [{:keys [result]
         {[[{:keys [images]} _ opts]] :read-receipt
          :keys [close]} :calls}
        (read-receipt receipt :provider ::base64/base64 :model "m")]
    (is (= ::hlp/result result)
        "The inner reader result is returned")
    (is (= 1 (count images))
        "The inner reader is given the image")
    (is (= (seq (.readAllBytes (io/input-stream receipt)))
           (seq (decode (first images))))
        "The image is base64-encoded as it is")
    (is (= "m" (:model opts))
        "The options are passed to the inner reader")
    (is (= 1 close)
        "The inner reader is closed")))

(deftest a-statement-reader-can-be-given-the-pages-base64-encoded
  (let [{:keys [result]
         {[[contents _ opts]] :read-statement
          :keys [close]} :calls}
        (read-statement {:page-images [(.getBytes "page 1")
                                       (.getBytes "page 2")]}
                        :provider ::base64/base64
                        :model "m")]
    (is (= ::hlp/result result)
        "The inner reader result is returned")
    (is (= ["page 1" "page 2"]
           (map #(String. (decode %)) (:images contents)))
        "Each page is base64-encoded")
    (is (= [:images] (keys contents))
        "The pages are given as images")
    (is (= "m" (:model opts))
        "The options are passed to the inner reader")
    (is (= 1 close)
        "The inner reader is closed")))

(deftest the-text-of-a-statement-is-passed-on
  (let [{{[[contents]] :read-statement} :calls}
        (read-statement {:text "Prairie State Bank"}
                        :provider ::base64/base64)]
    (is (= {:text "Prairie State Bank"} contents))))

(deftest the-pages-of-a-scanned-pdf-can-be-scaled-and-encoded
  (let [inner (inspectable-reader)]
    (ing/read-statement (ing/statement-reader
                          {::ing/provider ::pdf/pdf
                           :dpi 72
                           :reader {::ing/provider ::scale/scale
                                    :max-image-dimension 100
                                    :reader {::ing/provider ::base64/base64
                                             :reader inner}}})
                        (blank-pdf 1)
                        {:id 1}
                        {})
    (is (= [[77 100]]
           (map #(let [image (ImageIO/read (ByteArrayInputStream. (decode %)))]
                   [(.getWidth image) (.getHeight image)])
                (get-in (hlp/calls inner) [:read-statement 0 0 :images])))
        "Each page is rendered, scaled, and encoded")))
