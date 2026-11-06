(ns clj-money.mime-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clj-money.mime :as mime]))

(defn- slurp-bytes
  [path]
  (.readAllBytes (io/input-stream path)))

(deftest parse-the-content-id-of-an-embedded-part
  (is (= [{:mime-type "text/html"}
          {:mime-type "image/png"
           :filename "logo.png"
           :embedded? true
           :content-id "logo@acme.example.com"}]
         (map #(dissoc % :content)
              (:parts (mime/parse (slurp-bytes "fixtures/emails/html_logo.eml")))))
      "The Content-ID is given without its angle brackets"))

(deftest show-the-images-of-an-email-in-place
  (let [{:keys [parts]} (mime/parse (slurp-bytes "fixtures/emails/html_logo.eml"))
        html (mime/inline-images (:content (first parts)) parts)]
    (is (re-find #"<img src=\"data:image/png;base64,iVBORw0KGgo[^\"]+\" alt=\"Acme\">" html)
        "An image from the email's own parts is given as a data URI")
    (is (not (re-find #"cid:" html))
        "An image the email doesn't have is left without a source")))

(deftest show-text-as-html
  (is (= "<html><body><pre>Milk    3.49\n&lt;Total&gt; 3.49</pre></body></html>"
         (mime/text->html "Milk    3.49\n<Total> 3.49"))
      "The text is escaped and keeps its layout"))
