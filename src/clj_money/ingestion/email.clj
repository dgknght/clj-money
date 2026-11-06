(ns clj-money.ingestion.email
  "Reads receipts sent by email. The email decorator is configured outside
  the rest of the receipt readers, as an email is uploaded in place of a
  photo of the receipt, e.g.:

    {:clj-money.ingestion/provider :clj-money.ingestion.email/email
     :reader {:clj-money.ingestion/provider :clj-money.ingestion.orient/orient
              :reader ...}}

  A receipt attached to the email (an image or a PDF) is read as an uploaded
  one would be. Otherwise, the text of the email is read."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clj-money.io :refer [read-bytes]]
            [clj-money.mime :as mime]
            [clj-money.pdf :as pdf]
            [clj-money.ingestion :as ing]))

(defn- of-type
  [mime-type parts]
  (some #(when (= mime-type (:mime-type %)) (:content %))
        parts))

(defn- body
  "Returns the text of the email, from its plain text part or, if it has
  none, its HTML part"
  [parts]
  (if-let [plain (of-type "text/plain" parts)]
    (str/trim plain)
    (some-> (of-type "text/html" parts)
            mime/html->text)))

(defn- receipt?
  "Returns true if the part is an image or a PDF, unless it's one the
  email's body displays, like a logo, which may be marked as an attachment
  all the same"
  [{:keys [mime-type embedded?]}]
  (and (not embedded?)
       (or (str/starts-with? mime-type "image/")
           (= "application/pdf" mime-type))))

(defn attachments
  "Returns the images and PDFs attached to the email, each with its
  :mime-type, :content (bytes) and, if it has one, :filename, leaving out
  any the email's body displays, like a logo"
  [content]
  (filterv receipt? (:parts (mime/parse content))))

(defn- body-html
  "Returns the HTML of the email's body, from its HTML part, with the images
  it shows from the email's own parts given in place, or, if it has none,
  from its plain text part"
  [parts]
  (if-let [html (of-type "text/html" parts)]
    (mime/inline-images html parts)
    (some-> (of-type "text/plain" parts)
            mime/text->html)))

(defn body-pdf
  "Returns a PDF (bytes) of the email's body as it's displayed, for an email
  the receipt is in the body of, i.e., one with no image or PDF attached,
  so that the receipt can be viewed without opening the email. Returns nil
  for an email with a receipt attached, or with no body, or if the body
  can't be rendered."
  [content]
  (let [{:keys [parts]} (mime/parse content)]
    (when-not (some receipt? parts)
      (when-let [html (body-html parts)]
        (try
          (pdf/render-html html)
          (catch Exception e
            (log/warn e "[email] unable to render the body of the email")
            nil))))))

(defn- contents
  "Returns what's read of the email: the first receipt attached to it or,
  if there's none, its text"
  [content]
  (let [{:keys [subject from parts]} (mime/parse content)]
    (or (some #(when (receipt? %) (:content %)) parts)
        {:text (str/join "\n" [(str "Subject: " subject)
                               (str "From: " (str/join ", " from))
                               ""
                               (body parts)])
         :origin :email})))

(defn- reader
  "Decorates a receipt reader, so that it's given, for an email, the receipt
  attached to it or the email's text, {:text \"...\" :origin :email}. A
  receipt that isn't an email is passed on as it is."
  [reader]
  (reify
    ing/ReceiptReader
    (read-receipt [_ source entity opts]
      (let [content (read-bytes source)]
        (ing/read-receipt reader
                          (if (mime/message? content)
                            (contents content)
                            content)
                          entity
                          opts)))
    ing/Closeable
    (close [_] (ing/close reader))))

(defmethod ing/receipt-reader ::email
  [{inner :reader}]
  {:pre [inner]}
  (reader (ing/receipt-reader inner)))
