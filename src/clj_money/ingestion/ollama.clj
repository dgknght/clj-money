(ns clj-money.ingestion.ollama
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [cheshire.core :as json]
            [cheshire.parse :as json-parse]
            [lambdaisland.uri :as uri]
            [clj-http.client :as http]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.ingestion :as ing])
  (:import java.util.Base64
           [java.awt Color RenderingHints]
           java.awt.image.BufferedImage
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           javax.imageio.ImageIO))

(defn ->base64
  [input]
  (let [input (io/input-stream input)]
    (.encodeToString (Base64/getEncoder)
                     (.readAllBytes input))))

(defn- read-bytes
  ^bytes [input]
  (with-open [in (io/input-stream input)]
    (.readAllBytes in)))

(defn- resize
  ^BufferedImage [^BufferedImage image width height]
  (let [width (int width)
        height (int height)
        resized (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        g (.createGraphics resized)]
    (try
      (.setRenderingHint g
                         RenderingHints/KEY_INTERPOLATION
                         RenderingHints/VALUE_INTERPOLATION_BICUBIC)
      ; fill first, so any transparency becomes white instead of black
      (.setColor g Color/WHITE)
      (.fillRect g 0 0 width height)
      (.drawImage g image 0 0 width height nil)
      resized
      (finally
        (.dispose g)))))

(defn scale-image
  "Scales the image down so that its longest side is no longer than
  max-dimension, returning PNG bytes. The model's image token count grows
  with the pixel count, so this keeps the prompt small. The bytes are
  returned unchanged if the image is already small enough, or can't be
  decoded as an image (e.g., a PDF)."
  ^bytes [^bytes image max-dimension]
  (if-let [decoded (ImageIO/read (ByteArrayInputStream. image))]
    (let [width (.getWidth decoded)
          height (.getHeight decoded)
          scale (/ max-dimension (max width height))]
      (if (< scale 1)
        (let [out (ByteArrayOutputStream.)]
          (ImageIO/write (resize decoded
                                 (max 1 (Math/round (double (* scale width))))
                                 (max 1 (Math/round (double (* scale height)))))
                         "png"
                         out)
          (.toByteArray out))
        image))
    image))

(defn image-for-model
  "Returns the base64-encoded image, scaled down for the model"
  [source {:keys [max-image-dimension]
           :or {max-image-dimension 1280}}]
  (->base64 (scale-image (read-bytes source) max-image-dimension)))

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
  base64-encoded image."
  [image prompt schema {:keys [model
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
                     seed (assoc :seed seed))
          :images [image]}
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
  (throw (ex-info "Error accessing the ollama service." {:source source})))

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
                  :raise false
                  :as :json
                  :body (json/generate-string req-body)
                  :socket-timeout timeout-ms
                  :connection-timeout timeout-ms})
      (select-keys [:status :body])))

(defn- read-receipt*
  [source entity opts]
  {:pre [entity]}

  (let [req-body (request-body (image-for-model source opts)
                               (rcpts/prompt entity)
                               (rcpts/schema entity)
                               opts)
        {:keys [status body]} (generate req-body opts)]
    (if (<= 200 status 299)
      (handle-success-response body req-body)
      (handle-failure-response body source))))

(defmethod ing/reader ::ollama
  [config]
  (reify ing/Reader
    (read-receipt
      [_ source entity opts]
      (read-receipt* source
                     entity
                     (merge config opts)))
    (close [_])))
