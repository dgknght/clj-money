(ns clj-money.ingestion.gemini-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [clj-http.client :as http]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.gemini :as gemini]))

(def ^:private jpeg
  (byte-array (map unchecked-byte [0xFF 0xD8 0xFF 0xE0 0x00])))

(deftest the-content-type-is-judged-by-the-leading-bytes
  (is (= "image/jpeg" (gemini/content-type jpeg)))
  (is (= "image/png" (gemini/content-type
                       (byte-array (map unchecked-byte [0x89 0x50 0x4E 0x47 0x0D])))))
  (is (= "application/pdf" (gemini/content-type (.getBytes "%PDF-1.7"))))
  (is (= "image/webp" (gemini/content-type (.getBytes "RIFF\u0000\u0000\u0000\u0000WEBPVP8 "))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Unsupported image type"
                        (gemini/content-type (.getBytes "GIF89a")))))

(deftest the-request-includes-the-image-prompt-and-schema
  (let [body (gemini/request-body jpeg "Read this" {:type "object"} {})]
    (is (= [{:inlineData {:mimeType "image/jpeg"
                          :data "/9j/4AA="}}
            {:text "Read this"}]
           (get-in body [:contents 0 :parts])))
    (is (= {:text {:mimeType "APPLICATION_JSON"
                   :schema {:type "object"}}}
           (get-in body [:generationConfig :responseFormat])))
    (is (= 8192 (get-in body [:generationConfig :maxOutputTokens]))
        "Output tokens are limited by default")
    (is (= {:thinkingLevel "LOW"}
           (get-in body [:generationConfig :thinkingConfig]))
        "Thinking is kept low by default")))

(deftest the-thinking-level-can-be-set-or-omitted
  (is (= {:thinkingLevel "MINIMAL"}
         (get-in (gemini/request-body jpeg "Read this" {} {:thinking-level :minimal})
                 [:generationConfig :thinkingConfig])))
  (is (nil? (get-in (gemini/request-body jpeg "Read this" {} {:thinking-level nil})
                    [:generationConfig :thinkingConfig]))
      "It can be left out for models before Gemini 3"))

(def ^:private success-body
  {:candidates [{:content {:parts [{:text "{\"total\": 38.97, \"location_name\": \"Kroger\"}"}]}
                 :finishReason "STOP"}]
   :usageMetadata {:promptTokenCount 1800
                   :candidatesTokenCount 250
                   :thoughtsTokenCount 50}
   :modelVersion "gemini-3.6-flash"})

(deftest the-receipt-is-read-from-the-response
  (let [result (gemini/handle-success-response success-body)]
    (is (= {:total 38.97M
            :location-name "Kroger"}
           result))
    (is (decimal? (:total result))
        "Amounts are decimals")
    (is (= {:model "gemini-3.6-flash"
            :input-tokens 1800
            :output-tokens 300}
           (::ing/usage (meta result)))
        "Token usage is attached, counting thinking tokens as output")))

(deftest an-incomplete-response-is-an-error
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"ran out of output tokens"
                        (gemini/handle-success-response
                          {:candidates [{:content {:parts [{:text "{\"tot"}]}
                                         :finishReason "MAX_TOKENS"}]})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"did not return a receipt"
                        (gemini/handle-success-response
                          {:promptFeedback {:blockReason "SAFETY"}}))))

(deftest read-a-receipt
  (let [calls (atom [])
        reader (ing/reader {::ing/provider ::gemini/gemini
                            :api-key "secret"
                            :model "gemini-test"})]
    (with-redefs [http/post (fn [url req]
                              (swap! calls conj [url req])
                              {:status 200 :body success-body})]
      (is (= 38.97M
             (:total (ing/read-receipt reader
                                       jpeg
                                       {:id 1}
                                       {:account-options {:payment-accounts ["Discover"]}}))))
      (let [[url req] (first @calls)
            body (json/parse-string (:body req) true)]
        (is (= "https://generativelanguage.googleapis.com/v1beta/models/gemini-test:generateContent"
               url))
        (is (= "secret" (get-in req [:headers "x-goog-api-key"])))
        (is (false? (:throw-exceptions req))
            "An error status is returned rather than thrown, so the API's error message is logged")
        (is (= ["Discover"]
               (get-in body [:generationConfig :responseFormat :text :schema
                             :properties :payment_account :enum]))
            "The account options constrain the response")))))

(deftest a-failed-request-is-an-error
  (let [reader (ing/reader {::ing/provider ::gemini/gemini
                            :api-key "secret"})]
    (with-redefs [http/post (constantly {:status 400
                                         :body {:error {:message "Bad key"}}})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Error accessing the Gemini API. Bad key"
                            (ing/read-receipt reader jpeg {:id 1} {}))
          "The API's error message is included"))))

(defn- respond-in-turn
  "Returns a stub for http/post that returns the given statuses in turn,
  and an atom counting the calls"
  [& statuses]
  (let [calls (atom 0)]
    [(fn [_url _req]
       (let [status (nth statuses (min @calls (dec (count statuses))))]
         (swap! calls inc)
         (if (= 200 status)
           {:status 200 :body success-body}
           {:status status
            :body {:error {:message "High demand"}}})))
     calls]))

(deftest a-busy-model-is-retried
  (let [reader (ing/reader {::ing/provider ::gemini/gemini
                            :api-key "secret"})
        [post calls] (respond-in-turn 503 429 200)
        pauses (atom [])]
    (with-redefs [http/post post
                  gemini/pause #(swap! pauses conj %)]
      (is (= 38.97M (:total (ing/read-receipt reader jpeg {:id 1} {}))))
      (is (= 3 @calls))
      (is (= [2000 4000] @pauses)
          "The delay doubles between attempts"))))

(deftest retries-are-limited
  (let [reader (ing/reader {::ing/provider ::gemini/gemini
                            :api-key "secret"
                            :max-attempts 2})
        [post calls] (respond-in-turn 503)]
    (with-redefs [http/post post
                  gemini/pause (constantly nil)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"High demand"
                            (ing/read-receipt reader jpeg {:id 1} {})))
      (is (= 2 @calls)))))

(deftest a-bad-request-is-not-retried
  (let [reader (ing/reader {::ing/provider ::gemini/gemini
                            :api-key "secret"})
        [post calls] (respond-in-turn 400)]
    (with-redefs [http/post post
                  gemini/pause (constantly nil)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (ing/read-receipt reader jpeg {:id 1} {})))
      (is (= 1 @calls)))))

(deftest an-api-key-is-required
  (is (thrown? AssertionError
               (ing/reader {::ing/provider ::gemini/gemini}))))
