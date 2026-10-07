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

(defn- ->base64
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

(defn- request-body
  [image entity {:keys [model
                        num-ctx
                        no-gpu]
                 :or {model "qwen2.5vl:7b"
                      num-ctx 8192}}]
  {:model model
   :prompt (rcpts/prompt entity)
   :stream false
   :format (rcpts/schema entity)
   :options (cond-> {:temperature 0
                     :num_ctx num-ctx}
              no-gpu (assoc :num_gpu 0))
   :images [image]})

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

(defn- read-receipt*
  [source entity opts]
  {:pre [entity]}

  (let [req-body (-> source
                     ->base64
                     (request-body entity opts))
        req {:content-type "application/json"
             :accept "application/json"
             :raise false
             :as :json
             :body (json/generate-string req-body)}
        _ (log/debugf "request: %s"
                      (with-out-str
                        (pprint (update-in req-body [:images] count))))
        {:keys [status body]} (http/post (url opts)
                                         req)]
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
