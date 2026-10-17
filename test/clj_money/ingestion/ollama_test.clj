(ns clj-money.ingestion.ollama-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [clj-http.core]
            [clojure.java.io :as io]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.ollama :as ollama])
  (:import java.awt.image.BufferedImage
           java.io.ByteArrayOutputStream
           javax.imageio.ImageIO))

(def ^:private handle-success-response
  #'clj-money.ingestion.ollama/handle-success-response)

(deftest amounts-in-the-response-are-read-as-decimals
  (let [result (handle-success-response
                 {:response "{\"total\": 38.97, \"line_items\": [{\"amount\": 30.30}]}"}
                 {:options {:num_ctx 4096}})]
    (is (= 38.97M (:total result))
        "The total is a decimal")
    (is (decimal? (get-in result [:line-items 0 :amount]))
        "The line item amounts are decimals")))

(deftest token-usage-is-attached-to-the-result
  (let [result (handle-success-response
                 {:response "{\"total\": 38.97}"
                  :model "qwen2.5vl:7b"
                  :prompt_eval_count 1200
                  :eval_count 300}
                 {:options {:num_ctx 4096}})]
    (is (= {:model "qwen2.5vl:7b"
            :input-tokens 1200
            :output-tokens 300}
           (::ing/usage (meta result))))))

(defn- image-bytes
  [width height]
  (let [out (ByteArrayOutputStream.)]
    (ImageIO/write (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
                   "jpg"
                   out)
    (.toByteArray out)))

(deftest output-tokens-are-limited-by-default
  (is (= 1500 (get-in (ollama/request-body ["image"] "prompt" {} {})
                      [:options :num_predict]))))

(defn- read-receipt
  [source config]
  (let [calls (atom [])]
    (with-redefs [http/post (fn [_url req]
                              (swap! calls conj req)
                              {:status 200
                               :body {:response "{\"total\": 38.97}"}})]
      (ing/read-receipt (ing/receipt-reader (assoc config
                                                   ::ing/provider ::ollama/ollama))
                        source
                        {:id 1}
                        {:account-options {:payment-accounts ["Discover"]
                                           :expense-accounts ["Groceries"]}}))
    (json/parse-string (:body (first @calls)) true)))

(deftest the-account-options-constrain-the-response
  (let [schema (:format (read-receipt {:text "SPROUTS"} {}))]
    (is (= ["Discover"]
           (get-in schema [:properties :payment_account :enum]))
        "The payment account is one of the payment account options")
    (is (= ["Groceries"]
           (get-in schema [:properties :line_items :items
                           :properties :account :enum]))
        "The line item account is one of the expense account options")))

(deftest the-text-of-a-receipt-is-sent-to-the-text-model
  (let [body (read-receipt {:text "SPROUTS FARMERS MARKET\nTOTAL 38.97"} {})]
    (is (re-find #"SPROUTS FARMERS MARKET" (:prompt body))
        "The text of the receipt follows the prompt")
    (is (nil? (:images body))
        "No images are sent")
    (is (= "qwen3:8b" (:model body))
        "A text model is used by default")
    (is (false? (:think body))
        "Thinking is off, so it doesn't use up the output tokens")))

(deftest the-images-of-a-receipt-are-sent-to-the-vision-model
  (let [body (read-receipt {:images ["aW1hZ2U="]} {:vision-model "llava:7b"})]
    (is (= ["aW1hZ2U="] (:images body))
        "The images are sent")
    (is (= "llava:7b" (:model body))
        "The vision model is used")))

(deftest an-image-itself-is-sent-to-the-vision-model
  ; for a config that doesn't decorate the reader, see clj-money.ingestion.image
  (let [body (read-receipt (image-bytes 800 600) {})]
    (is (= 1 (count (:images body)))
        "The image is sent")
    (is (= "qwen2.5vl:7b" (:model body))
        "The vision model is used")))

(deftest a-receipt-without-contents-cant-be-read
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"must be given the contents of the receipt"
                        (ollama/receipt-request-body {} {:id 1} {}))))

(def ^:private statement-text
  {:text "Prairie State Bank\n09/04 City Power & Light 134.22"})

(defn- read-statement
  [source config]
  (let [calls (atom [])]
    (with-redefs [http/post (fn [_url req]
                              (swap! calls conj req)
                              {:status 200
                               :body {:response "{\"transactions\": []}"}})]
      (ing/read-statement (ing/statement-reader (assoc config
                                                       ::ing/provider ::ollama/ollama))
                          source
                          {:id 1}
                          {:account-options {:statement-accounts ["Checking"]
                                             :category-accounts ["Groceries"]}}))
    (json/parse-string (:body (first @calls)) true)))

(deftest the-text-of-a-statement-is-sent-to-the-model
  (let [body (read-statement statement-text {})]
    (is (re-find #"Prairie State Bank" (:prompt body))
        "The text of the statement follows the prompt")
    (is (nil? (:images body))
        "No images are sent")
    (is (= "qwen3:8b" (:model body))
        "A text model is used by default")
    (is (= {:num_ctx 32768 :num_predict 8192}
           (select-keys (:options body) [:num_ctx :num_predict]))
        "The context and output are large enough for a statement")
    (is (false? (:think body))
        "Thinking is off, so it doesn't use up the output tokens")
    (is (= ["Checking"]
           (get-in body [:format :properties :account :enum]))
        "The account options constrain the response")))

(deftest a-scanned-statement-is-sent-as-images-to-the-vision-model
  (let [body (read-statement {:page-images [(image-bytes 800 600)
                                            (image-bytes 800 600)]}
                             {})]
    (is (= 2 (count (:images body)))
        "Each page is sent as an image")
    (is (= "qwen2.5vl:7b" (:model body))
        "The vision model is used")))

(deftest the-statement-defaults-can-be-configured
  (let [body (read-statement statement-text
                             {:model "qwen3:14b"
                              :num-ctx 16384})]
    (is (= "qwen3:14b" (:model body)))
    (is (= 16384 (get-in body [:options :num_ctx])))))

(deftest an-error-from-the-service-is-explained
  (with-redefs [clj-http.core/request (fn [_req]
                                        {:status 404
                                         :headers {"content-type" "application/json"}
                                         :body (java.io.ByteArrayInputStream.
                                                (.getBytes "{\"error\":\"model 'qwen3:8b' not found\"}"))})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"returned an error: model 'qwen3:8b' not found"
                          (ing/read-statement (ing/statement-reader {::ing/provider ::ollama/ollama})
                                              statement-text
                                              {:id 1}
                                              {})))))

(deftest a-pdf-itself-cant-be-read
  ; it must be given the pdf's contents, see clj-money.ingestion.pdf
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"can't read a PDF"
                        (ollama/statement-request-body
                          (io/file "fixtures/statements/checking.pdf")
                          {:id 1}
                          {}))))
