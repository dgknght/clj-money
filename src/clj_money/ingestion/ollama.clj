(ns clj-money.ingestion.ollama
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [lambdaisland.uri :as uri]
            [clj-http.client :as http]
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

(defn- read-receipt*
  [source {:as opts :keys [model] :or {model  "qwen2.5vl:7b"}}]
  (let [req {:content-type "application/json"
             :accept "application/json"
             :as :json
             :body (json/generate-string
                     {:model model
                      :prompt "This is a purchase receipt. Extract the date, location, payment method, and total amount."
                      :stream false
                      :format "json"
                      :images [(->base64 source)]})}
        {:keys [status body]} (http/post (url opts)
                                         req)]
    (if (<= 200 status 299)
      body
      (throw (ex-info "Error accessing the ollama service." {:source source})))))

(defmethod ing/reader ::ollama
  [config]
  (reify ing/Reader
    (read-receipt [_ source] (read-receipt* source config))))
