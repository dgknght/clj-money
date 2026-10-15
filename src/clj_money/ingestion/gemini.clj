(ns clj-money.ingestion.gemini
  "Reads receipts with the Google Gemini API"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [cheshire.core :as json]
            [cheshire.parse :as json-parse]
            [clj-http.client :as http]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.ingestion :as ing])
  (:import java.util.Base64))

(defn- read-bytes
  ^bytes [input]
  (with-open [in (io/input-stream input)]
    (.readAllBytes in)))

(defn- starts-with?
  [^bytes content & signature]
  (and (<= (count signature) (alength content))
       (every? (fn [[i b]]
                 (= (unchecked-byte b) (aget content i)))
               (map-indexed vector signature))))

(defn content-type
  "Returns the MIME type of the image, judged by its leading bytes, as the
  API needs it and the image's stored content type isn't passed to the
  reader."
  [^bytes content]
  (cond
    (starts-with? content 0xFF 0xD8 0xFF) "image/jpeg"
    (starts-with? content 0x89 0x50 0x4E 0x47) "image/png"
    (starts-with? content 0x25 0x50 0x44 0x46) "application/pdf" ; %PDF
    (and (starts-with? content 0x52 0x49 0x46 0x46)                ; RIFF
         (= "WEBP" (String. content 8 4 "US-ASCII"))) "image/webp"
    :else (throw (ex-info "Unsupported image type" {}))))

(defn request-body
  "Returns the body of a generateContent request asking the model to read
  the image."
  [^bytes image prompt schema {:keys [max-output-tokens thinking-level]
                               :or {; thinking tokens count against this
                                    ; limit, so it leaves room for both
                                    max-output-tokens 8192
                                    ; reading a receipt needs little
                                    ; reasoning, and less is faster
                                    thinking-level "LOW"}}]
  {:contents [{:parts [{:inlineData {:mimeType (content-type image)
                                     :data (.encodeToString (Base64/getEncoder)
                                                            image)}}
                       {:text prompt}]}]
   :generationConfig (cond-> {:temperature 0.2
                              :maxOutputTokens max-output-tokens
                              :responseFormat {:text {:mimeType "APPLICATION_JSON"
                                                      :schema schema}}}
                       ; models before Gemini 3 reject a thinking level
                       thinking-level (assoc :thinkingConfig
                                             {:thinkingLevel (str/upper-case
                                                               (name thinking-level))}))})

(defn- url
  [{:keys [model]
    :or {model "gemini-3.6-flash"}}]
  (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
          model))

(defn- post
  [req-body {:keys [api-key timeout-ms]
             :or {timeout-ms 120000} ; 2 minutes
             :as opts}]
  (-> (http/post (url opts)
                 {:content-type "application/json"
                  :accept "application/json"
                  :headers {"x-goog-api-key" api-key}
                  :throw-exceptions false
                  :as :json
                  ; decode error responses too, for the API's error message
                  :coerce :always
                  :body (json/generate-string req-body)
                  :socket-timeout timeout-ms
                  :connection-timeout timeout-ms})
      (select-keys [:status :body])))

; The API answers 429 when the rate limit is reached, and 503 when the
; model is overloaded, both of which pass if the request is retried later
(def ^:private retryable? #{429 500 503})

(defn- pause
  [ms]
  (Thread/sleep (long ms)))

(defn- generate
  "Posts the request, retrying with exponential backoff if the API is
  temporarily unavailable."
  [req-body {:keys [max-attempts retry-delay-ms]
             :or {max-attempts 3
                  retry-delay-ms 2000}
             :as opts}]
  (loop [attempt 1
         delay-ms retry-delay-ms]
    (let [{:keys [status] :as response} (post req-body opts)]
      (if (and (retryable? status)
               (< attempt max-attempts))
        (do
          (log/warnf "The Gemini API responded %s, retrying in %sms" status delay-ms)
          (pause delay-ms)
          (recur (inc attempt) (* 2 delay-ms)))
        response))))

(defn handle-success-response
  "Returns the receipt read from the response, with the token usage attached
  as metadata. Throws if the model didn't return a receipt (e.g., the image
  was blocked, or the output was cut off)."
  [{:keys [candidates promptFeedback usageMetadata modelVersion] :as body}]
  (log/debugf "result: %s" (with-out-str (pprint body)))
  (let [{:keys [finishReason content]} (first candidates)
        text (some :text (:parts content))]
    (when (= "MAX_TOKENS" finishReason)
      (throw (ex-info "The model ran out of output tokens before finishing the receipt."
                      {:finish-reason finishReason
                       :usage usageMetadata})))
    (when-not (and text (= "STOP" finishReason))
      (throw (ex-info "The model did not return a receipt."
                      {:finish-reason finishReason
                       :block-reason (:blockReason promptFeedback)})))
    ; amounts must be decimals, as doubles can't be rounded to cents
    (binding [json-parse/*use-bigdecimals?* true]
      (vary-meta (json/parse-string text ->kebab-case-keyword)
                 assoc
                 ::ing/usage
                 {:model modelVersion
                  :input-tokens (:promptTokenCount usageMetadata)
                  ; thinking tokens are billed as output
                  :output-tokens (some->> [(:candidatesTokenCount usageMetadata)
                                           (:thoughtsTokenCount usageMetadata)]
                                          (filter some?)
                                          seq
                                          (reduce +))}))))

(defn- handle-failure-response
  [status body]
  (log/errorf "Error accessing the Gemini API (%s): %s" status body)
  (let [message (get-in body [:error :message])]
    (throw (ex-info (cond-> "Error accessing the Gemini API."
                      message (str " " message))
                    {:status status
                     :error message}))))

(defn- read-receipt*
  [source entity {:keys [account-options] :as opts}]
  {:pre [entity]}
  (let [{:keys [status body]} (generate (request-body (read-bytes source)
                                                      (rcpts/prompt entity)
                                                      (rcpts/schema account-options)
                                                      opts)
                                        opts)]
    (if (<= 200 status 299)
      (handle-success-response body)
      (handle-failure-response status body))))

(defmethod ing/reader ::gemini
  [{:keys [api-key] :as config}]
  {:pre [api-key]}
  (reify ing/Reader
    (read-receipt
      [_ source entity opts]
      (read-receipt* source
                     entity
                     (merge config opts)))
    (close [_])))
