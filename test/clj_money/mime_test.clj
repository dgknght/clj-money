(ns clj-money.mime-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clj-money.io :refer [read-bytes]]
            [clj-money.mime :as mime]))

(defn- email
  [filename]
  (read-bytes (io/file "fixtures/emails" filename)))

(deftest parse-the-headers-of-an-email
  (is (= {:subject "Your Kroger receipt"
          :message-id "<plain-1@kroger.example.com>"
          :from ["Kroger <receipts@kroger.example.com>"]
          :senders ["receipts@kroger.example.com"]}
         (select-keys (mime/parse (email "plain.eml"))
                      [:subject :message-id :from :senders]))))

(deftest parse-the-parts-of-an-email
  (let [{:keys [parts]} (mime/parse (email "attachment.eml"))]
    (is (= ["text/plain" "text/html" "application/pdf"]
           (map :mime-type parts)))
    (is (= "receipt.pdf" (:filename (last parts))))))

(deftest recognize-an-email
  (is (mime/message? (email "plain.eml")))
  (is (not (mime/message? (read-bytes (io/file "fixtures/attachment.jpg"))))))
