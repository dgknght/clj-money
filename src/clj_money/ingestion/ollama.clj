(ns clj-money.ingestion.ollama
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
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
  [image entity {:keys [model num-ctx]
                 :or {model "qwen2.5vl:7b"
                      num-ctx 8192}}]
  {:model model
   :prompt (rcpts/prompt entity)
   :stream false
   :format (rcpts/schema entity)
   :options {:temperature 0
             :num_ctx num-ctx}
   :images [image]})

(defn- read-receipt*
  [source entity opts]
  (let [req-body (-> source
                       ->base64
                       (request-body entity opts))
        req {:content-type "application/json"
             :accept "application/json"
             :as :json
             :body (json/generate-string req-body)}
        {:keys [status body]} (http/post (url opts)
                                         req)]
    (if (<= 200 status 299)
      (do
        (log/debugf "format: %s" (pr-str (:format req-body)))
        (log/debugf "result: %s" (with-out-str (pprint (dissoc body :context))))
        (when (<= (get-in req-body [:options :num_ctx])
                  (:prompt_eval_count body 0))
          (log/warnf "The prompt filled the context window (%s tokens) and may have been truncated"
                     (:prompt_eval_count body)))
        (-> body
            :response
            (json/parse-string true)))
      (do
        (log/errorf "Error accessing the ollama service: %s" body)
        (throw (ex-info "Error accessing the ollama service." {:source source}))))))

(defmethod ing/reader ::ollama
  [config]
  (reify ing/Reader
    (read-receipt
      [_ source entity]
      (read-receipt* source
                     entity
                     config))
    (close [_] (println "shut down the ingestion component."))))
