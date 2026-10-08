(ns clj-money.ingestion.ollama
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [cheshire.core :as json]
            [lambdaisland.uri :as uri]
            [clj-http.client :as http]
            [clj-money.ingestion.receipts :as rcpts]
            [clj-money.ingestion :as ing])
  (:import java.util.Base64))

(defn ->base64
  [input]
  (let [input (io/input-stream input)]
    (.encodeToString (Base64/getEncoder)
                     (.readAllBytes input))))

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
                             num-ctx 8192}}]
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
  (-> body
      :response
      (json/parse-string ->kebab-case-keyword)))

(defn- handle-failure-response
  [body source]
  (log/errorf "Error accessing the ollama service: %s" body)
  (throw (ex-info "Error accessing the ollama service." {:source source})))

(defn generate
  "Posts the request body to the ollama generate endpoint and returns the
  status and the decoded body."
  [req-body {:keys [timeout-ms] :as opts}]
  (log/debugf "request: %s"
              (with-out-str
                (pprint (update-in req-body [:images] count))))
  (-> (http/post (url opts)
                 (cond-> {:content-type "application/json"
                          :accept "application/json"
                          :raise false
                          :as :json
                          :body (json/generate-string req-body)}
                   timeout-ms (assoc :socket-timeout timeout-ms
                                     :connection-timeout timeout-ms)))
      (select-keys [:status :body])))

(defn- read-receipt*
  [source entity opts]
  {:pre [entity]}

  (let [req-body (request-body (->base64 source)
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
