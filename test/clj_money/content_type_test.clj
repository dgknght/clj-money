(ns clj-money.content-type-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.content-type :as ct]))

(deftest the-content-type-is-judged-by-the-leading-bytes
  (is (= "image/jpeg" (ct/sniff (byte-array (map unchecked-byte [0xFF 0xD8 0xFF 0xE0 0x00])))))
  (is (= "image/png" (ct/sniff (byte-array (map unchecked-byte [0x89 0x50 0x4E 0x47 0x0D])))))
  (is (= "application/pdf" (ct/sniff (.getBytes "%PDF-1.7"))))
  (is (= "image/webp" (ct/sniff (.getBytes "RIFF\u0000\u0000\u0000\u0000WEBPVP8 "))))
  (is (nil? (ct/sniff (.getBytes "GIF89a"))))
  (is (nil? (ct/sniff (.getBytes "RIFF")))
      "Content too short to hold a signature isn't recognized"))
