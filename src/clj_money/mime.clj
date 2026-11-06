(ns clj-money.mime
  "Reads email messages (MIME, as saved in an .eml file) into plain data,
  with Jakarta Mail, and the text of their HTML bodies, with jsoup"
  (:require [clojure.string :as str]
            [clj-money.io :refer [read-bytes]]
            [clj-money.base64 :as b64])
  (:import java.io.ByteArrayInputStream
           java.util.Properties
           [jakarta.mail Multipart Part Session]
           [jakarta.mail.internet ContentType MimeMessage]
           org.jsoup.Jsoup
           org.jsoup.nodes.Entities))

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

(defn- content-id
  "Returns the part's Content-ID without the angle brackets around it, as a
  cid: URL in the HTML body names it"
  [^Part part]
  (some->> (.getHeader part "Content-ID")
           first
           str/trim
           (re-find #"^<?(.*?)>?$")
           second))

(defn- ->part
  [^Part part]
  (let [mime-type (str/lower-case (.getBaseType (ContentType. (.getContentType part))))
        cid (content-id part)]
    (cond-> {:mime-type mime-type
             :content (if (str/starts-with? mime-type "text/")
                        (.getContent part)
                        (read-bytes (.getInputStream part)))}
      (.getFileName part) (assoc :filename (.getFileName part))
      cid (assoc :embedded? true
                 :content-id cid))))

(defn parse
  "Returns the message's :subject, :from (the senders' addresses, as
  strings), and :parts, each part that holds content with its :mime-type
  (e.g., \"text/plain\") and its :content, a string for a text part, and
  bytes for any other, and its :filename, if it has one. A part with a
  Content-ID, which the HTML body can display (e.g., a logo), is
  :embedded?, and has its :content-id."
  [^bytes content]
  (let [message (MimeMessage. (Session/getInstance (Properties.))
                              (ByteArrayInputStream. content))]
    {:subject (.getSubject message)
     :from (mapv str (.getFrom message))
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

(defn- data-uri
  [{:keys [mime-type content]}]
  (str "data:" mime-type ";base64," (b64/encode content)))

(defn inline-images
  "Returns the HTML with each image it shows from the email's own parts
  (src=\"cid:...\") given in place as a data URI, so that it can be shown
  without the email"
  [html parts]
  (let [doc (Jsoup/parse ^String html)
        by-cid (->> parts
                    (filter :content-id)
                    (map (juxt :content-id identity))
                    (into {}))]
    (doseq [element (.select doc "[src^=cid:]")]
      (if-let [part (by-cid (subs (.attr element "src") 4))]
        (.attr element "src" ^String (data-uri part))
        (.removeAttr element "src")))
    (.outerHtml doc)))

(defn text->html
  "Returns an HTML document that shows the text as it's laid out"
  [text]
  (str "<html><body><pre>" (Entities/escape ^String text) "</pre></body></html>"))
