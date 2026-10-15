(ns clj-money.ingestion.ollama-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.ollama :as ollama])
  (:import java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
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

(defn- dimensions
  [image]
  (let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    [(.getWidth decoded) (.getHeight decoded)]))

(deftest a-large-image-is-scaled-down-for-the-model
  (is (= [960 1280]
         (dimensions (ollama/scale-image (image-bytes 1536 2048) 1280)))
      "The longest side is scaled to the maximum, keeping the aspect ratio"))

(deftest a-small-image-is-sent-unchanged
  (let [image (image-bytes 800 600)]
    (is (identical? image (ollama/scale-image image 1280)))))

(deftest a-file-that-isnt-an-image-is-sent-unchanged
  (let [pdf (.getBytes "%PDF-1.7")]
    (is (identical? pdf (ollama/scale-image pdf 1280)))))

(deftest output-tokens-are-limited-by-default
  (is (= 1500 (get-in (ollama/request-body "image" "prompt" {} {})
                      [:options :num_predict]))))

(deftest the-account-options-constrain-the-response
  (let [calls (atom [])
        reader (ing/reader {::ing/provider ::ollama/ollama})]
    (with-redefs [http/post (fn [_url req]
                              (swap! calls conj req)
                              {:status 200
                               :body {:response "{\"total\": 38.97}"}})]
      (ing/read-receipt reader
                        (image-bytes 800 600)
                        {:id 1}
                        {:account-options {:payment-accounts ["Discover"]
                                           :expense-accounts ["Groceries"]}})
      (let [schema (:format (json/parse-string (:body (first @calls)) true))]
        (is (= ["Discover"]
               (get-in schema [:properties :payment_account :enum]))
            "The payment account is one of the payment account options")
        (is (= ["Groceries"]
               (get-in schema [:properties :line_items :items
                               :properties :account :enum]))
            "The line item account is one of the expense account options")))))
