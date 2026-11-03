(ns clj-money.mime
  "Reads email messages (MIME, as saved in an .eml file) into plain data,
  with Jakarta Mail, and the text of their HTML bodies, with jsoup"
  (:require [clojure.string :as str]
            [clj-money.io :refer [read-bytes]])
  (:import java.io.ByteArrayInputStream
           java.util.Properties
           [jakarta.mail Multipart Part Session]
           [jakarta.mail.internet ContentType InternetAddress MimeMessage]
           org.jsoup.Jsoup))

(defn message?
  "Returns true if the content starts with a header field, as a message
  does, and an image or a PDF never does, or with the \"From \" line of a
  message saved from a mailbox file"
  [^bytes content]
  (boolean
    (re-find #"^(?:[!-9;-~]+:|From )"
             (String. content 0 (int (min 1000 (alength content))) "US-ASCII"))))

(defn- leaf-parts
  "Returns the parts of the message that hold content, i.e., all but the
  multiparts that contain them"
  [^Part part]
  (if (.isMimeType part "multipart/*")
    (let [^Multipart multipart (.getContent part)]
      (mapcat #(leaf-parts (.getBodyPart multipart (int %)))
              (range (.getCount multipart))))
    [part]))

(defn- ->part
  [^Part part]
  (let [mime-type (str/lower-case (.getBaseType (ContentType. (.getContentType part))))]
    (cond-> {:mime-type mime-type
             :content (if (str/starts-with? mime-type "text/")
                        (.getContent part)
                        (read-bytes (.getInputStream part)))}
      (.getFileName part) (assoc :filename (.getFileName part))
      (seq (.getHeader part "Content-ID")) (assoc :embedded? true))))

(defn- sender-address
  [address]
  (str/lower-case (if (instance? InternetAddress address)
                    (.getAddress ^InternetAddress address)
                    (str address))))

(defn parse
  "Returns the message's :subject, :message-id, :from (the senders, as
  strings, e.g., \"Store <receipts@store.com>\"), :senders (just their
  addresses, in lower case), and :parts, each part that holds content with
  its :mime-type (e.g., \"text/plain\") and its :content, a string for a
  text part, and bytes for any other, and its :filename, if it has one. A
  part with a Content-ID, which the HTML body can display (e.g., a logo), is
  :embedded?."
  [^bytes content]
  (let [message (MimeMessage. (Session/getInstance (Properties.))
                              (ByteArrayInputStream. content))]
    {:subject (.getSubject message)
     :message-id (.getMessageID message)
     :from (mapv str (.getFrom message))
     :senders (mapv sender-address (.getFrom message))
     :parts (mapv ->part (leaf-parts message))}))

(def ^:private block-elements
  "br, p, div, tr, li, table, h1, h2, h3, h4, h5, h6")

(defn html->text
  "Returns the text of the HTML, with each block element (paragraph, table
  row, etc.) on a line of its own, and the cells of a table row apart"
  [html]
  (let [doc (Jsoup/parse ^String html)]
    (.remove (.select doc "style, script, head"))
    (doseq [element (.select doc "td, th")]
      (.after element " "))
    (doseq [element (.select doc block-elements)]
      (.after element "\n"))
    (->> (str/split-lines (.wholeText doc))
         (map str/trim)
         (remove str/blank?)
         (str/join "\n"))))
