(ns clj-money.ingestion.email-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clj-money.ingestion.email :as email]
            [clj-money.ingestion.helpers :refer [read-receipt]]))

(defn- slurp-bytes
  [path]
  (.readAllBytes (io/input-stream path)))

(defn- read-email
  "Returns what the inner reader is given when the email reader reads the
  receipt"
  [receipt]
  (get-in (read-receipt receipt :provider ::email/email)
          [:calls :read-receipt 0 0]))

(deftest read-the-text-of-a-plain-text-email
  (let [{:keys [text origin]} (read-email (io/file "fixtures/emails/plain.eml"))]
    (is (= :email origin) "The text is identified as an email's")
    (is (re-find #"^Subject: Your Kroger receipt\nFrom: Kroger <receipts@kroger.example.com>\n\n" text)
        "The subject and sender lead the text")
    (is (re-find #"Total {9}6\.48" text)
        "The body of the email follows, as it's laid out")))

(deftest read-the-text-of-an-html-email
  (is (= (str "Subject: Your Kroger receipt\n"
              "From: Kroger <receipts@kroger.example.com>\n"
              "\n"
              "Thank you for shopping at Kroger!\n"
              "Milk 3.49\n"
              "Total 3.49")
         (:text (read-email (io/file "fixtures/emails/html.eml"))))
      "The text of the HTML is read, each paragraph and table row on a line of its own, without styles"))

(deftest read-a-receipt-attached-to-an-email
  (is (= "%PDF-1.4 fake receipt"
         (String. ^bytes (read-email (io/file "fixtures/emails/attachment.eml"))
                  "US-ASCII"))
      "The receipt attached is read in place of the email's text"))

(deftest skip-an-image-the-email-displays
  ; a logo shown in the HTML body is sent as a part with a Content-ID, and
  ; may even be marked as an attachment
  (is (= "%PDF-1.4 fake invoice"
         (String. ^bytes (read-email (io/file "fixtures/emails/embedded_image.eml"))
                  "US-ASCII"))
      "The receipt attached is read, not the image shown in the email's body"))

(deftest list-the-files-attached-to-an-email
  (is (= [{:mime-type "application/pdf"
           :filename "invoice.pdf"
           :content "%PDF-1.4 fake invoice"}]
         (map #(update-in % [:content] (fn [b] (String. ^bytes b "US-ASCII")))
              (email/attachments (slurp-bytes "fixtures/emails/embedded_image.eml"))))
      "Each image or PDF attached is listed, but not the logo the email displays"))

(deftest read-an-email-saved-from-a-mailbox-file
  (let [content (.getBytes (str "From - Mon Nov 02 09:15:00 2026\n"
                                (slurp "fixtures/emails/plain.eml"))
                           "US-ASCII")]
    (is (re-find #"Total {9}6\.48"
                 (:text (read-email content)))
        "The leading \"From \" line is skipped")))

(deftest pass-on-a-receipt-that-isnt-an-email
  (let [image (io/file "fixtures/sample_receipt.jpg")]
    (is (= (seq (.readAllBytes (io/input-stream image)))
           (seq (read-email image)))
        "The image is passed on as it is")))

(deftest close-the-inner-reader
  (is (= 1 (get-in (read-receipt (io/file "fixtures/emails/plain.eml")
                                 :provider ::email/email)
                   [:calls :close]))))
