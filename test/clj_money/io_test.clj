(ns clj-money.io-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-money.io :as io]))

(deftest extract-file-name
  (let [tests [{:input "/home/name/file.txt"
                :expected "file.txt"}
               {:input "/home/name/file.tar.gz"
                :expected "file.tar.gz"}]]
    (doseq [{:keys [input expected]} tests]
      (testing (format "Given \"%s\", it should return \"%s\"" input expected)
        (is (= expected (io/file-name input))
            "The correct value is returned")))))

(deftest extract-file-extension
  (let [tests [{:input "/home/name/file.txt"
                :expected "txt"}
               {:input "/home/name/file.tar.gz"
                :expected "tar.gz"}]]
    (doseq [{:keys [input expected]} tests]
      (testing (format "Given \"%s\", it should return \"%s\"" input expected)
        (is (= expected (io/file-ext input))
            "The correct value is returned")))))
