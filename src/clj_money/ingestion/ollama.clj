(ns clj-money.ingestion.ollama
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [cheshire.core :as json]
            [cheshire.parse :as json-parse]
            [lambdaisland.uri :as uri]
            [clj-http.client :as http]
            [clj-money.ingestion.image :refer [->base64
                                               image-for-model
                                               scale-image]]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.ingestion.statements :as stmts]
            [clj-money.ingestion :as ing]))

(defn- url
  [{:keys [host
           port
           scheme]
    :or {host "localhost"
         port 11434
         scheme "http"}}]
  (-> (uri/parse "/api/generate")
      (assoc :host host
             :port port
             :scheme scheme)
      uri/uri-str))

(defn request-body
  "Returns the body of a generate request asking the model to read the
  base64-encoded images. With no images, the prompt itself holds what is to
  be read."
  [images prompt schema {:keys [model
                               num-ctx
                               num-gpu
                               num-predict
                               seed
                               think]
                        :or {model "qwen2.5vl:7b"
                             num-ctx 8192
                             ; stops a model that's repeating itself, rather
                             ; than letting it run until the request times out
                             num-predict 1500}}]
  (cond-> {:model model
          :prompt prompt
          :stream false
          :format schema
          :options (cond-> {:temperature 0.2
                            :top_k 20
                            :top_p 0.8
                            :num_ctx num-ctx}
                     num-gpu (assoc :num_gpu num-gpu)
                     num-predict (assoc :num_predict num-predict)
                     seed (assoc :seed seed))}
    (seq images) (assoc :images (vec images))
    (some? think) (assoc :think think)))

(defn- handle-success-response
  [body req-body]
  (log/debugf "result: %s" (with-out-str (pprint (dissoc body :context))))
  (when (<= (get-in req-body [:options :num_ctx])
            (:prompt_eval_count body 0))
    (log/warnf "The prompt filled the context window (%s tokens) and may have been truncated"
               (:prompt_eval_count body)))
  ; amounts must be decimals, as doubles can't be rounded to cents
  (binding [json-parse/*use-bigdecimals?* true]
    (some-> body
            :response
            (json/parse-string ->kebab-case-keyword)
            (vary-meta assoc ::ing/usage {:model (:model body)
                                          :input-tokens (:prompt_eval_count body)
                                          :output-tokens (:eval_count body)}))))

(defn- handle-failure-response
  [body source]
  (log/errorf "Error accessing the ollama service: %s" body)
  (throw (ex-info (if-let [error (:error body)]
                    (format "The ollama service returned an error: %s" error)
                    "Error accessing the ollama service.")
                  {:source source})))

(defn generate
  "Posts the request body to the ollama generate endpoint and returns the
  status and the decoded body."
  [req-body {:keys [timeout-ms]
             :or {timeout-ms 300000} ; 5 minutes
             :as opts}]
  (log/debugf "request: %s"
              (with-out-str
                (pprint (update-in req-body [:images] count))))
  (-> (http/post (url opts)
                 {:content-type "application/json"
                  :accept "application/json"
                  :throw-exceptions false
                  :as :json
                  ; ollama explains an error in a json body, e.g.,
                  ; {"error": "model 'qwen3:8b' not found"}
                  :coerce :always
                  :body (json/generate-string req-body)
                  :socket-timeout timeout-ms
                  :connection-timeout timeout-ms})
      (select-keys [:status :body])))

(defn- send-request
  [req-body source opts]
  (let [{:keys [status body]} (generate req-body opts)]
    (if (<= 200 status 299)
      (handle-success-response body req-body)
      (handle-failure-response body source))))

; A receipt read by OCR is sent as text to a text model, and one sent as an
; image to the vision model
(def receipt-defaults
  {:model "qwen3:8b"
   :vision-model "qwen2.5vl:7b"
   ; with thinking on, the reasoning uses up the output tokens
   :think false})

(defn receipt-request-body
  "Returns the body of a generate request asking the model to read the
  receipt, given its contents: {:text \"...\"}, the text read by OCR (see
  clj-money.ingestion.ocr), sent with the prompt to the text model, or
  {:images [base64 ...]} (see clj-money.ingestion.image), sent to the vision
  model."
  [{:keys [text images]} entity {:keys [account-options vision-model] :as opts}]
  (let [prompt (rcpts/prompt entity)
        schema (rcpts/schema account-options)]
    (cond
      text
      (request-body []
                    (str prompt
                         "\n\nThe text of the receipt, read from the image by OCR, follows.\n\n"
                         text)
                    schema
                    opts)

      images
      (request-body images
                    prompt
                    schema
                    (assoc opts :model vision-model))

      :else
      (throw (ex-info (str "The ollama reader must be given the contents of the receipt. "
                           "Configure it under the :reader of a "
                           ":clj-money.ingestion.ocr/ocr or "
                           ":clj-money.ingestion.image/image reader.")
                      {})))))

(defn- receipt-contents
  "Returns the contents of the receipt, accepting the image itself, as the
  reader did before it was given the contents, so that a config without a
  decorator keeps working"
  [source opts]
  (if (map? source)
    source
    (do
      (log/warn (str "The ollama receipt reader was given an image. Configure it "
                     "under the :reader of a :clj-money.ingestion.image/image reader."))
      {:images [(image-for-model source opts)]})))

(defn- read-receipt*
  [source entity opts]
  {:pre [entity]}
  (let [opts (merge receipt-defaults opts)]
    (send-request (receipt-request-body (receipt-contents source opts) entity opts)
                  ; not the contents, which would put the receipt in the log
                  :receipt
                  opts)))

; A statement is much longer than a receipt, both the text read and the
; answer, and is better read as text than as images, which a text model
; handles better than a vision model of the same size
(def statement-defaults
  {:model "qwen3:8b"
   :vision-model "qwen2.5vl:7b"
   :num-ctx 32768
   :num-predict 8192
   :max-image-dimension 1600
   ; with thinking on, the reasoning uses up the output tokens
   :think false})

(defn statement-request-body
  "Returns the body of a generate request asking the model to read the
  statement, given the contents of its PDF (see
  clj-money.ingestion/StatementReader), as the models can't open a PDF.
  The text is sent with the prompt. The images of the pages of a PDF without
  text (e.g., one scanned from paper) are sent to the vision model instead."
  [{:keys [text page-images]} entity {:keys [account-options vision-model] :as opts}]
  (let [prompt (stmts/prompt entity)
        schema (stmts/schema account-options)]
    (cond
      text
      (request-body []
                    (str prompt
                         "\n\nThe text of the statement, extracted from the PDF, follows.\n\n"
                         text)
                    schema
                    opts)

      page-images
      (request-body (map #(->base64 (scale-image % (:max-image-dimension opts)))
                         page-images)
                    prompt
                    schema
                    (assoc opts :model vision-model))

      :else
      (throw (ex-info (str "The ollama models can't read a PDF, only its contents. "
                           "Configure the ollama reader under the :reader of a "
                           ":clj-money.ingestion.pdf/pdf reader.")
                      {})))))

(defn- read-statement*
  [contents entity opts]
  {:pre [entity]}
  (let [opts (merge statement-defaults opts)]
    (send-request (statement-request-body contents entity opts)
                  ; not the contents, which would put the statement in the log
                  :statement
                  opts)))

(defmethod ing/receipt-reader ::ollama
  [config]
  (reify
    ing/ReceiptReader
    (read-receipt
      [_ source entity opts]
      (read-receipt* source
                     entity
                     (merge config opts)))
    ing/Closeable
    (close [_])))

(defmethod ing/statement-reader ::ollama
  [config]
  (reify
    ing/StatementReader
    (read-statement
      [_ contents entity opts]
      (read-statement* contents
                       entity
                       (merge config opts)))
    ing/Closeable
    (close [_])))
