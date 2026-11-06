(ns clj-money.pdf-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.pdf :as pdf])
  (:import [java.net ServerSocket SocketTimeoutException]))

(deftest render-html-as-a-pdf
  (let [{:keys [text page-count]} (pdf/read-text
                                    (pdf/render-html
                                      "<p>Thank you & goodbye<table><tr><td>Total</td><td>3.49</td></tr></table>"))]
    (is (= 1 page-count))
    (is (re-find #"Thank you & goodbye" text)
        "HTML that isn't well-formed is rendered")
    (is (re-find #"Total\s+3\.49" text))))

(defn- count-connections
  "Calls f with a URL served locally, and returns the number of
  connections made to it while f runs"
  [f]
  (with-open [server (doto (ServerSocket. 0)
                       (.setSoTimeout 100))]
    (let [url (str "http://127.0.0.1:" (.getLocalPort server))
          connections (atom 0)
          listener (future
                     (while (not (.isClosed server))
                       (try
                         (with-open [_ (.accept server)]
                           (swap! connections inc))
                         (catch SocketTimeoutException _)
                         (catch java.io.IOException _))))]
      (f url)
      (Thread/sleep 200)
      (.close server)
      @listener
      @connections)))

(deftest render-html-without-fetching-anything
  (is (zero? (count-connections
               (fn [url]
                 (pdf/render-html
                   (str "<html><head>"
                        "<link rel=\"stylesheet\" href=\"" url "/site.css\">"
                        "<style>@import url(" url "/more.css); "
                        "body { background: url(" url "/bg.png); }</style>"
                        "</head><body>"
                        "<img src=\"" url "/pixel.gif\">"
                        "<p>Your receipt</p>"
                        "</body></html>")))))
      "No stylesheet or image named by the HTML is requested"))

(deftest render-an-image-given-in-place
  (is (re-find #"Logo"
               (:text (pdf/read-text
                        (pdf/render-html
                          (str "<p>Logo</p><img src=\"data:image/png;base64,"
                               "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGP4z8AAAAMBAQDJ/pLvAAAAAElFTkSuQmCC"
                               "\">")))))
      "An image given as a data URI is rendered"))
