(ns clj-money.io-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.io :as io]))

(deftest extract-file-name
  (let [tests [{:input "/home/name/file.txt"
                :expected "file.txt"}
               {:input "/home/name/file.tar.gz"
                :expected "file.tar.gz"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (io/file-name input))
          (format "Given \"%s\", it returns \"%s\"" input expected)))))

(deftest extract-file-extension
  (let [tests [{:input "/home/name/file.txt"
                :expected "txt"}
               {:input "/home/name/file.tar.gz"
                :expected "tar.gz"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (io/file-ext input))
          (format "Given \"%s\", it returns \"%s\"" input expected)))))
