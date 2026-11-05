(ns clj-money.content-type
  "Judges the type of content, given as bytes, by its leading bytes")

(defn- starts-with?
  [^bytes content & signature]
  (and (<= (count signature) (alength content))
       (every? (fn [[i b]]
                 (= (unchecked-byte b) (aget content i)))
               (map-indexed vector signature))))

(defn- ascii
  [^bytes content offset length]
  (when (<= (+ offset length) (alength content))
    (String. content (int offset) (int length) "US-ASCII")))

(defn sniff
  "Returns the MIME type of the content, an image or a PDF, judged by its
  leading bytes, or nil if it's none of those recognized"
  [^bytes content]
  (cond
    (starts-with? content 0xFF 0xD8 0xFF) "image/jpeg"
    (starts-with? content 0x89 0x50 0x4E 0x47) "image/png"
    (= "%PDF" (ascii content 0 4)) "application/pdf"
    (and (= "RIFF" (ascii content 0 4))
         (= "WEBP" (ascii content 8 4))) "image/webp"))
