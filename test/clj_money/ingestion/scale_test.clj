(ns clj-money.ingestion.scale-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-money.ingestion.scale :as scale]
            [clj-money.ingestion.helpers :as hlp :refer [read-receipt
                                                          read-statement]])
  (:import java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           javax.imageio.ImageIO))

(defn- image-bytes
  [width height]
  (let [out (ByteArrayOutputStream.)]
    (ImageIO/write (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
                   "jpg"
                   out)
    (.toByteArray out)))

(defn- dimensions
  [image]
  (let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    [(.getWidth decoded) (.getHeight decoded)]))

(deftest shrink-an-image
  (testing "input is larger than the max"
    (is (= [960 1280]
           (dimensions (scale/shrink (image-bytes 1536 2048) 1280)))
        "The longest side is scaled to the maximum, keeping the aspect ratio"))
  (testing "input is smaller than the max"
    (let [image (image-bytes 800 600)]
      (is (= (seq image)
             (seq (scale/shrink image 1280))))))
  (testing "input is not an image"
    (let [pdf (.getBytes "%PDF-1.7")]
      (is (= (seq pdf)
             (seq (scale/shrink pdf 1280)))
          "The input is returned as-is"))))

(deftest enlarge-an-image
  (testing "input is smaller than the minimum"
    (is (= [2000 1500]
           (dimensions (scale/enlarge (image-bytes 800 600) 2000)))
        "The longest side is scaled to the minimum, keeping the aspect ratio"))
  (testing "input is larger than the minimum"
    (let [image (image-bytes 800 600)]
      (is (= (seq image)
             (seq (scale/enlarge image 800))))))
  (testing "input is not an image"
    (let [pdf (.getBytes "%PDF-1.7")]
      (is (= (seq pdf)
             (seq (scale/enlarge pdf 2000)))
          "The input is returned as is"))))

(deftest read-a-receipt
  (let [{:keys [result]
         {[[image _ opts]] :read-receipt
          :keys [close]} :calls}
        (read-receipt (image-bytes 400 200)
                      :provider ::scale/scale
                      :max-image-dimension 100)]
    (is (= ::hlp/result result)
        "The inner reader result is returned")
    (is (= [100 50] (dimensions image))
        "The inner reader is given the image scaled to the configured maximum")
    (is (= 100 (:max-image-dimension opts))
        "The options are passed to the inner reader")
    (is (= 1 close)
        "The inner reader is closed")))

(deftest read-a-statement
  (testing "an image statement"
    (let [{:keys [result]
           {[[{:keys [page-images]} _ opts]] :read-statement
            :keys [close]} :calls}
          (read-statement {:page-images [(image-bytes 400 200)
                                         (image-bytes 200 400)]}
                          :provider ::scale/scale
                          :max-image-dimension 100)]
      (is (= ::hlp/result result)
          "The inner reader result is returned")
      (is (= [[100 50] [50 100]] (map dimensions page-images))
          "Each page is scaled to the configured maximum")
      (is (= 100 (:max-image-dimension opts))
          "The options are passed to the inner reader")
      (is (= 1 close)
          "The inner reader is closed")))
  (testing "a text statement"
    (let [{{[[contents]] :read-statement} :calls}
          (read-statement {:text "Prairie State Bank"}
                          :provider ::scale/scale)]
      (is (= {:text "Prairie State Bank"} contents)))))
